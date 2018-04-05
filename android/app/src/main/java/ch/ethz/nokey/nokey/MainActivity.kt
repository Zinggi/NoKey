package ch.ethz.nokey.nokey

import android.app.Activity
import android.os.Bundle


class MainActivity : Activity() {
    private lateinit var webViewHelper: WebViewHelper
    private lateinit var uiManager: UIManager

    // TODO: Is it possible to change to "file:///android_asset/..."
    // and later, once online use that?
    // TODO! change before release
    // val appUrl = "https://virt35.ethz.ch/webApp"
    // for this to work run the dev server with yarn dev_ssl.
    // 10.0.3.2 is from genymotion and forwards to localhost
    val appUrl = "https://10.0.3.2:3001/main.html"

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
