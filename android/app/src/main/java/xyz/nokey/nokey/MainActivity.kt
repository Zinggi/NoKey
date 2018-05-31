package xyz.nokey.nokey

import android.Manifest
import android.app.Activity
import android.os.Bundle
import android.widget.Toast
import com.google.zxing.integration.android.IntentIntegrator
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Environment
import android.support.v4.app.ActivityCompat
import android.support.v4.content.ContextCompat
import android.util.Log
import android.webkit.ValueCallback
import android.webkit.WebChromeClient
import java.io.File
import kotlin.math.log


class MainActivity : Activity() {
    private lateinit var webViewHelper: WebViewHelper
    private lateinit var uiManager: UIManager

    val requestSelectFile = 100
    val scanQr = 200
    val writeExternalStoragePermissonRequestCode = 300
    var uploadMessage: ValueCallback<Array<Uri>>? = null


    // TODO: Is it possible to change to "file:///android_asset/..."
    // and later, once online use that?
    // TODO! change before release
    val appUrl = "https://nokey.xyz/webApp"
    val appBaseUrl =  "https://nokey.xyz"

    // for this to work run the dev server with yarn dev_ssl.
    // 10.0.3.2 is from genymotion and forwards to localhost
    // val appUrl = "https://10.0.3.2:3001/main.html"
    // val appBaseUrl = "https://10.0.3.2:3001"
    // local ip, to test on device
    // val appUrl = "https://10.2.121.215:3001/main.html"
    // val appBaseUrl = "https://10.2.121.215:3001"

    // TODO! change shell version if android apk changes
    val androidShellVersion: String = "0.4.0"

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        // TODO setTheme(R.style.AppTheme_NoActionBar)
        setContentView(R.layout.activity_main)

        uiManager = UIManager(this)
        webViewHelper = WebViewHelper(this, uiManager)

        webViewHelper.setupWebView()
        // TODO? uiManager.changeRecentAppsIcon()

        webViewHelper.loadHome()
    }

    fun scanQR() {
        val integrator = IntentIntegrator(this)
        integrator.setPrompt("Scan a QR code for pairing.")
        integrator.setRequestCode(scanQr)
        integrator.setOrientationLocked(false)
        integrator.setBeepEnabled(false)
        integrator.initiateScan()
    }

    var tmpFileName : String? = null
    var tmpContent : String? = null

    fun downloadTextFile(fileName: String, content: String) {
        if (ContextCompat.checkSelfPermission(this,
                        Manifest.permission.WRITE_EXTERNAL_STORAGE)
                != PackageManager.PERMISSION_GRANTED) {

            // Permission is not granted
            tmpContent = content
            tmpFileName = fileName
            // Should we show an explanation?
            if (ActivityCompat.shouldShowRequestPermissionRationale(this,
                            Manifest.permission.WRITE_EXTERNAL_STORAGE)) {
                Toast.makeText(this, "I need to be able to access the storage to save $fileName",
                        Toast.LENGTH_LONG).show()
            } else {
                // No explanation needed, we can request the permission.
                ActivityCompat.requestPermissions(this,
                        arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE),
                        writeExternalStoragePermissonRequestCode)
            }
        } else {
            // Permission has already been granted
            doWriteFile(fileName, content)
        }
    }

    private fun doWriteFile(fileName: String, content: String) {
        tmpContent = null
        tmpFileName = null
        val isWritable = Environment.getExternalStorageState() == Environment.MEDIA_MOUNTED
        if (!isWritable) return
        // Get the directory for the user's public download directory.
        val path = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS)
        val newFileName = getUniqueName(path, fileName)
        val file = File(path, newFileName)
        file.writeText(content)

        Toast.makeText(this, "Saved as $newFileName to ${path.absolutePath}",
                Toast.LENGTH_LONG).show()
    }

    private fun getUniqueName(path: File?, fileName: String, count: Int = 0): String? {
        val f = File(fileName)
        val ext = f.extension
        val name = f.nameWithoutExtension
        val actualName = if (count == 0) { fileName } else { "$name($count).$ext" }
        return if (File(path, actualName).exists()) {
            getUniqueName(path, fileName, count + 1)
        } else {
            actualName
        }

    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == requestSelectFile) {
            if (uploadMessage == null) return
            uploadMessage?.onReceiveValue(WebChromeClient.FileChooserParams.parseResult(resultCode, data))
            uploadMessage = null
        } else if (requestCode == scanQr) {
            if (data == null) {
                Toast.makeText(this, "Cancelled", Toast.LENGTH_LONG).show()
                return
            }
            val result = IntentIntegrator.parseActivityResult(resultCode, data)
            if (result != null) {
                if (result.contents == null) {
                    Toast.makeText(this, "Cancelled", Toast.LENGTH_LONG).show()
                } else {
                    Toast.makeText(this, "Scanned: " + result.contents, Toast.LENGTH_LONG).show()
                    webViewHelper.onScanQrResult(result.contents);
                }
            } else {
                Toast.makeText(this, "Invalid QR code", Toast.LENGTH_LONG).show()
            }
        } else {
            super.onActivityResult(requestCode, resultCode, data)
        }
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        when (requestCode) {
            writeExternalStoragePermissonRequestCode -> {
                // If request is cancelled, the result arrays are empty.
                if ((grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED)) {
                    // permission was granted, yay!
                    if (tmpContent != null && tmpFileName != null) {
                        doWriteFile(tmpFileName!!, tmpContent!!)
                    }
                } else {
                    // permission denied, boo!
                }
                return
            }
            else -> {
                // Ignore all other requests.
            }
        }
    }

    override fun onPause() {
        webViewHelper.onPause()
        super.onPause()
    }

    override fun onResume() {
        webViewHelper.onResume()
        // retrieve content from cache primarily if not connected
        webViewHelper.forceCacheIfOffline()
        super.onResume()
    }

    // Handle back-press in browser
    override fun onBackPressed() {
        if (!webViewHelper.goBack()) {
            super.onBackPressed()
        }
    }
}
