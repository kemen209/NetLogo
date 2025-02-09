// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.window

import java.awt.{ Component, Dialog, FileDialog => AWTFileDialog }
import java.awt.event.ActionEvent
import java.nio.file.Paths
import java.net.URI
import javax.swing.{ AbstractAction, JButton, JComponent, JDialog }

import org.nlogo.api.{ FileIO, ModelReader, ModelType, Version }, ModelReader.modelSuffix
import org.nlogo.awt.{ EventQueue, UserCancelException }
import org.nlogo.core.{ I18N, Model }
import org.nlogo.fileformat.{ ConversionError, ConversionWithErrors, ErroredConversion, FailedConversionResult }
import org.nlogo.swing.{ BrowserLauncher, FileDialog, MessageDialog, OptionDialog }
import org.nlogo.workspace.{ ModelTracker, OpenModel, SaveModel },
  OpenModel.{ Controller => OpenModelController },
  SaveModel.{ Controller => SaveModelController }

import scala.util.Try

class BackgroundFileController(dialog: JDialog, foregroundController: FileController) extends OpenModelController with SaveModelController {

  def runOnUIThread[A](f: () => Unit) =
    runOnUIThreadForResult(f)

  def runOnUIThreadForResult[A](f: () => A): A = {
    import scala.concurrent.{ Await, Promise }
    import scala.concurrent.duration.Duration
    val promise = Promise[A]()

    EventQueue.invokeLater { () =>
      dialog.setVisible(false)
      promise.complete(Try(f()))
      dialog.setVisible(true)
    }

    Await.result(promise.future, Duration.Inf)
  }

  // Members declared in org.nlogo.workspace.SaveModel.Controller
  def chooseFilePath(modelType: ModelType): Option[URI] =
    runOnUIThreadForResult(() => foregroundController.chooseFilePath(modelType))

  def shouldSaveModelOfDifferingVersion(version: String): Boolean =
    runOnUIThreadForResult(() => foregroundController.shouldSaveModelOfDifferingVersion(version))

  def warnInvalidFileFormat(format: String): Unit =
    runOnUIThread(() => foregroundController.warnInvalidFileFormat(format))

  // Members declared in org.nlogo.workspace.OpenModel.Controller
  def errorOpeningURI(uri: java.net.URI,exception: Exception): Unit =
    runOnUIThread(() => foregroundController.errorOpeningURI(uri, exception))

  def errorAutoconvertingModel(res: FailedConversionResult): Option[Model] =
    runOnUIThreadForResult(() => foregroundController.errorAutoconvertingModel(res))
  def invalidModel(uri: java.net.URI): Unit =
    runOnUIThread(() => foregroundController.invalidModel(uri))
  def invalidModelVersion(uri: java.net.URI,version: String): Unit =
    runOnUIThread(() => foregroundController.invalidModelVersion(uri, version))
  def shouldOpenModelOfDifferingArity(arity: Int,version: String): Boolean =
    runOnUIThreadForResult(() => foregroundController.shouldOpenModelOfDifferingArity(arity, version))
  def shouldOpenModelOfLegacyVersion(version: String): Boolean =
    runOnUIThreadForResult(() => foregroundController.shouldOpenModelOfLegacyVersion(version))
  def shouldOpenModelOfUnknownVersion(version: String): Boolean =
    runOnUIThreadForResult(() => foregroundController.shouldOpenModelOfUnknownVersion(version))
}

class FileController(owner: Component, modelTracker: ModelTracker) extends OpenModelController with SaveModelController {
  // OpenModel.Controller methods
  def errorOpeningURI(uri: URI, exception: Exception): Unit = {
    val options = Array[Object](I18N.gui.get("common.buttons.ok"))
    println(exception)
    exception.printStackTrace()
    OptionDialog.showMessage(owner, "NetLogo",
      I18N.gui.getN("file.open.error.unableToOpen",
        Paths.get(uri).toString, exception.getMessage),
      options)
    throw new UserCancelException()
  }

  def errorAutoconvertingModel(res: FailedConversionResult): Option[Model] =
    showAutoconversionError(res, "model")

  def showAutoconversionError(res: FailedConversionResult, base: String): Option[Model] = {
    res.errors.foreach(_.errors.foreach { e =>
      println(e)
      e.printStackTrace()
    })
    val dialog = new AutoConversionErrorDialog(owner, base)
    dialog.doShow(res)
    dialog.modelToOpen
  }

  @throws(classOf[IllegalStateException])
  def invalidModel(uri: URI): Unit = {
    notifyUserNotValidFile(uri)
  }

  def invalidModelVersion(uri: URI, version: String): Unit = {
    notifyUserNotValidFile(uri)
  }

  def shouldOpenModelOfDifferingArity(arity: Int, version: String): Boolean = {
    try {
      if (arity == 3)
        checkWithUserBeforeOpening3DModelin2D(version)
      else
        checkWithUserBeforeOpening2DModelin3D()
      true
    } catch {
      case ex: UserCancelException => false
    }
  }

  def shouldOpenModelOfUnknownVersion(version: String): Boolean = {
    try {
      checkWithUserBeforeOpeningModelFromFutureVersion(version);
      true
    } catch {
      case ex: UserCancelException => false
    }
  }

  def shouldOpenModelOfLegacyVersion(version: String): Boolean = {
    showVersionWarningAndGetResponse(version)
  }

  lazy val continueAndCancelOptions = Array[Object](
    I18N.gui.get("common.buttons.continue"),
    I18N.gui.get("common.buttons.cancel"))

  @throws(classOf[UserCancelException])
  def checkWithUserBeforeOpeningModelFromFutureVersion(version: String): Unit = {
    val message = I18N.gui.getN("file.open.warn.version.newer", Version.version, version)
    if (OptionDialog.showMessage(owner, "NetLogo", message, continueAndCancelOptions) != 0) {
      throw new UserCancelException()
    }
  }

  @throws(classOf[UserCancelException])
  def checkWithUserBeforeOpening3DModelin2D(version: String): Unit = {
    val message = I18N.gui.getN("file.open.warn.intwod.openthreed", Version.version, version)
    if (OptionDialog.showMessage(owner, "NetLogo", message, continueAndCancelOptions) != 0) {
      throw new UserCancelException()
    }
  }

  @throws(classOf[UserCancelException])
  def checkWithUserBeforeOpening2DModelin3D(): Unit = {
    val message = I18N.gui.getN("file.open.warn.inthreed.opentwod", Version.version)
    if (OptionDialog.showMessage(owner, "NetLogo", message, continueAndCancelOptions) != 0) {
      throw new UserCancelException()
    }
  }

  @throws(classOf[UserCancelException])
  def notifyUserNotValidFile(uri: URI): Unit = {
    val warningText = Try(Paths.get(uri))
      .toOption
      .map(path => I18N.gui.getN("file.open.error.invalidmodel.withPath", path.toString))
      .getOrElse(I18N.gui.get("file.open.error.invalidmodel"))
    val options = Array[Object](I18N.gui.get("common.buttons.ok"))
    OptionDialog.showMessage(owner, "NetLogo", warningText, options)
    throw new UserCancelException()
  }

  def showVersionWarningAndGetResponse(version: String): Boolean = {
    val message = I18N.gui.getN("file.open.warn.version.older", version, Version.version)
    val options = Array[Object](
      I18N.gui.get("common.buttons.continue"),
      I18N.gui.get("file.open.warn.version.transitionGuide"),
      I18N.gui.get("common.buttons.cancel"))
    val response =
      OptionDialog.showMessage(owner, I18N.gui.get("common.messages.warning"), message, options)
    response match {
      case 0 => true
      case 1 =>
        BrowserLauncher.openURI(owner, new URI(I18N.gui.get("file.open.transitionGuide.url")))
        showVersionWarningAndGetResponse(version)
      case 2 => false
    }
  }

  def chooseFilePath(modelType: org.nlogo.api.ModelType): Option[java.net.URI] = {
    val newFileName = guessFileName

    // we only default to saving in the model dir for normal
    // models. for library and new models, we use the current
    // FileDialog dir.
    if (modelTracker.getModelType == ModelType.Normal) {
      FileDialog.setDirectory(modelTracker.getModelDir)
    }

    val userPath = FileDialog.showFiles(
      owner, I18N.gui.get("menu.file.saveAs"), AWTFileDialog.SAVE,
      newFileName, List[String](ModelReader.modelSuffix))
    val extensionPath = FileIO.ensureExtension(userPath, modelSuffix)
    val path = Paths.get(extensionPath)
    if (!path.toFile.exists) {
      Some(path.toUri)
    } else {
      FileDialog.confirmFileOverwrite(owner, extensionPath).map((_) => path.toUri)
    }
  }

  /**
   * makes a guess as to what the user would like to save this model as.
   * This is the model name if there is one, "Untitled.nlogo" otherwise.
   */
  private def guessFileName: String =
    FileIO.ensureExtension(modelTracker.modelNameForDisplay, modelSuffix)

  def shouldSaveModelOfDifferingVersion(version: String): Boolean = {
    Version.compatibleVersion(version) || {
      val options = Array[Object](
        I18N.gui.get("common.buttons.save"),
        I18N.gui.get("common.buttons.cancel"))
      val message = I18N.gui.getN("file.save.warn.savingInNewerVersion", version, Version.version)
      OptionDialog.showMessage(owner, "NetLogo", message, options) == 0
    }
  }

  def warnInvalidFileFormat(format: String): Unit = {
    val options = Array[Object](I18N.gui.get("common.buttons.ok"))
    val message = I18N.gui.getN("file.save.warn.invalidFormat", format)
    OptionDialog.showMessage(owner, I18N.gui.get("common.messages.warning"), message, options)
  }
}

class AutoConversionErrorDialog(owner: Component, keyContext: String) extends MessageDialog(owner, I18N.gui.get("common.buttons.cancel")) {
  setModalityType(Dialog.ModalityType.DOCUMENT_MODAL)

  var modelToOpen = Option.empty[Model]

  class ConversionAction(name: String) extends AbstractAction(name) {
    val ModelKey = "ConversionModel"
    def putModel(model: Model): Unit = putValue(ModelKey, model)
    def actionPerformed(e: ActionEvent): Unit = {
      AutoConversionErrorDialog.this.modelToOpen =
        Option(getValue(ModelKey).asInstanceOf[Model])
      setVisible(false)
    }
  }

  lazy val bestEffortAction = new ConversionAction(I18N.gui.get(s"file.open.warn.autoconversion.$keyContext.bestEffort"))
  lazy val originalAction = new ConversionAction(I18N.gui.get(s"file.open.warn.autoconversion.$keyContext.original"))

  override def makeButtons(): Seq[JComponent] = {
    super.makeButtons() ++ Seq(new JButton(bestEffortAction), new JButton(originalAction))
  }

  def errorMessage(failure: FailedConversionResult): String =
    I18N.gui.get(s"file.open.warn.autoconversion.$keyContext.error") +
      failure.errors.map(decorateError(_)).mkString("\n\n", "\n", "")

  private def decorateError(error: ConversionError): String = {
    val errorMessages = error.errors.map(e => s"- ${e.getMessage}").mkString("\n", "\n", "")
    I18N.gui.getN(s"file.open.warn.autoconversion.$keyContext.detail",
      error.conversionDescription, error.componentDescription, errorMessages)
  }

  def doShow(failure: FailedConversionResult): Unit = {
    modelToOpen = None
    failure match {
      case ErroredConversion(original, _) =>
        bestEffortAction.setEnabled(false)
        originalAction.putModel(original)
      case ConversionWithErrors(original, bestEffort, _) =>
        bestEffortAction.setEnabled(true)
        originalAction.putModel(original)
        bestEffortAction.putModel(bestEffort)
    }
    doShow(I18N.gui.get(s"file.open.warn.autoconversion.$keyContext.title"), errorMessage(failure), 5, 50)
  }
}
