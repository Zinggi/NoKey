// Some code here was borrowed from:
// https://github.com/xtools-at/Android-PWA-Wrapper

package ch.ethz.nokey.nokey

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.net.ConnectivityManager
import android.os.Build
import android.annotation.TargetApi
import android.graphics.Bitmap
import android.os.Message
import android.webkit.*
import android.content.Intent
import android.webkit.WebView
import android.webkit.WebViewClient
import android.app.AlertDialog
import android.net.Uri
import android.os.Handler
import android.content.pm.ApplicationInfo
import android.util.Log
import java.security.PublicKey
import android.webkit.ValueCallback




class WebViewHelper(private val activity: Activity) {
    private val webView: WebView
    private val webSettings: WebSettings

    // TODO: change to "file:///android_asset/..."
    private val appUrl = "http://virt35.ethz.ch/webApp"

    init {
        this.webView = activity.findViewById(R.id.webView)
        this.webSettings = webView.settings
    }

    private fun isNetworkAvailable(): Boolean {
        val manager = activity.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager
        val networkInfo = manager.activeNetworkInfo

        // Wifi or Mobile Network is present and connected
        return networkInfo != null && networkInfo.isConnected
    }

    // manipulate cache settings to make sure our web app gets updated
    private fun useCache(use: Boolean) {
        if (use) {
            webSettings.cacheMode = WebSettings.LOAD_CACHE_ELSE_NETWORK
        } else {
            webSettings.cacheMode = WebSettings.LOAD_DEFAULT
        }
    }

    // public method changing cache settings according to network availability.
    // retrieve content from cache primarily if not connected,
    // allow fetching from web too otherwise to get updates.
    fun forceCacheIfOffline() {
        useCache(!isNetworkAvailable())
    }

    @SuppressLint("SetJavaScriptEnabled")
// handles initial setup of webview
    fun setupWebView() {
        // accept cookies
        CookieManager.getInstance().setAcceptCookie(true)

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
            if (0 != activity.applicationInfo.flags and ApplicationInfo.FLAG_DEBUGGABLE) {
                WebView.setWebContentsDebuggingEnabled(true)
            }
        }


        webSettings.apply {
            // enable JS
            javaScriptEnabled = true
            // TODO? must be set for our js-popup-blocker:
            setSupportMultipleWindows(true)

            // PWA settings TODO?
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.KITKAT) {
                databasePath = activity.applicationContext?.filesDir?.absolutePath ?: ""
            }
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
                setAppCacheMaxSize(Long.MAX_VALUE)
            }
            domStorageEnabled = true
            setAppCachePath(activity.applicationContext.cacheDir.absolutePath)

            setAppCacheEnabled(true)
            databaseEnabled = true

            // TODO? turn on/off mixed content (both https+http within one page) for API >= 21
            val enableMixedContent =  false
            if (enableMixedContent && Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                mixedContentMode = WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE
            }
        }

        // retrieve content from cache primarily if not connected
        forceCacheIfOffline()

        // TODO? set User Agent
//        if (shouldOVERRIDE_USER_AGENT || shouldAppendPOSTFIX_USER_AGENT) {
//            var userAgent = ""
//            if (ShouldOVERRIDE_USER_AGENT) {
//                userAgent = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Mobile Safari/537.36"
//            }
//            if (ShouldAppendPOSTFIX_USER_AGENT) {
//                // useful for identifying traffic, e.g. in Google Analytics
//                userAgent = userAgent + " " + "AndroidApp";
//            }
//            webSettings.setUserAgentString(userAgent)
//        }

        // enable HTML5-support
        webView.webChromeClient = object : WebChromeClient() {
            //simple yet effective redirect/popup blocker
            // TODO? needed???
            override fun onCreateWindow(view: WebView, isDialog: Boolean, isUserGesture: Boolean, resultMsg: Message): Boolean {
                val href = view.handler.obtainMessage()
                view.requestFocusNodeHref(href)
                val popupUrl = href.data.getString("url")
                if (popupUrl != null) {
                    //it's null for most rouge browser hijack ads
                    webView.loadUrl(popupUrl)
                    return true
                }
                return false
            }

        }

        // Set up Webview client
        webView.webViewClient = object : WebViewClient() {
            override fun onPageStarted(view: WebView, url: String, favicon: Bitmap?) {
                super.onPageStarted(view, url, favicon)
                handleUrlLoad(view, url)
            }

            // handle loading error by showing the offline screen
            @Deprecated("")
            override fun onReceivedError(view: WebView, errorCode: Int, description: String, failingUrl: String) {
                if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M) {
                    handleLoadError(errorCode)
                }
            }

            @TargetApi(Build.VERSION_CODES.M)
            override fun onReceivedError(view: WebView, request: WebResourceRequest, error: WebResourceError) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                    // new API method calls this on every error for each resource.
                    // we only want to interfere if the page itself got problems.
                    val url = request.url.toString()
                    if (view.url == url) {
                        handleLoadError(error.errorCode)
                    }
                }
            }
        }

        // Now we can call these functions from javascript
        webView.addJavascriptInterface(object {
            @JavascriptInterface
            fun test() {
                Log.d("sdsd", "sdsdsd")
                // communicate back to JS
                webView.loadUrl("javascript:fromAndroid(\"lloldsds\"");
                // new and better way:
                webView.evaluateJavascript("(function() { return 'this'; })();", { s ->
                    Log.d("sdsd", s) // Prints: "this"
                })
            }
        }, "Android")
    }

    // show "no app found" dialog
    private fun showNoAppDialog(thisActivity: Activity?) {
        AlertDialog.Builder(thisActivity)
                .setTitle(R.string.noapp_heading)
                .setMessage(R.string.noapp_description)
                .show()
    }

    // handle load errors
    private fun handleLoadError(errorCode: Int) {
        if (errorCode != WebViewClient.ERROR_UNSUPPORTED_SCHEME) {
            // uiManager.setOffline(true)
            // TODO?
        } else {
            // Unsupported Scheme, recover
            Handler().postDelayed({ goBack() }, 100)
        }
    }

    // handle external urls
    private fun handleUrlLoad(view: WebView, url: String): Boolean {
        // prevent loading content that isn't ours
        if (!url.startsWith(appUrl)) {
            // stop loading
            view.stopLoading()

            // open external URL in Browser/3rd party apps instead
            try {
                val intent = Intent(Intent.ACTION_VIEW, Uri.parse(url))
                if (intent.resolveActivity(activity.packageManager) != null) {
                    activity.startActivity(intent)
                } else {
                    showNoAppDialog(activity)
                }
            } catch (e: Exception) {
                showNoAppDialog(activity)
            }

            // return value for shouldOverrideUrlLoading
            return true
        } else {
            // let WebView load the page!
            // activate loading animation screen
            // TODO?
            // uiManager.setLoading(true)
            // return value for shouldOverrideUrlLoading
            return false
        }
    }

    // handle back button press
    fun goBack(): Boolean {
        if (webView.canGoBack()) {
            webView.goBack()
            return true
        }
        return false
    }


    // load app startpage
    fun loadHome() {
        webView.loadUrl(appUrl)
    }

    // Lifecycle callbacks
    fun onPause() {
        webView.onPause()
    }

    fun onResume() {
        webView.onResume()
    }
}