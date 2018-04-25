package xyz.nokey.nokey

import android.app.Activity
import android.os.Bundle
import android.widget.Toast
import com.google.zxing.integration.android.IntentIntegrator
import com.google.zxing.integration.android.IntentResult
import android.content.Intent




class MainActivity : Activity() {
    private lateinit var webViewHelper: WebViewHelper
    private lateinit var uiManager: UIManager

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
        integrator.setOrientationLocked(false)
        integrator.setBeepEnabled(false)
        integrator.initiateScan()

        //IntentIntegrator(this).initiateScan()
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (data == null) {
            return
        }
        val result = IntentIntegrator.parseActivityResult(requestCode, resultCode, data)
        if (result != null) {
            if (result.contents == null) {
                Toast.makeText(this, "Cancelled", Toast.LENGTH_LONG).show()
            } else {
                Toast.makeText(this, "Scanned: " + result.contents, Toast.LENGTH_LONG).show()
                webViewHelper.onScanQrResult(result.contents);
            }
        } else {
            super.onActivityResult(requestCode, resultCode, data)
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
