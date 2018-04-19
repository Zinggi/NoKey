// Some code here was borrowed from:
// https://github.com/xtools-at/Android-PWA-Wrapper

package xyz.nokey.nokey

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.net.ConnectivityManager
import android.os.Build
import android.annotation.TargetApi
import android.graphics.Bitmap
import android.webkit.*
import android.content.Intent
import android.webkit.WebView
import android.webkit.WebViewClient
import android.app.AlertDialog
import android.net.Uri
import android.os.Handler
import android.content.pm.ApplicationInfo
import android.util.Log


class WebViewHelper(private val activity: MainActivity, private val uiManager: UIManager) {
    private val webView: WebView = activity.findViewById(R.id.webView)
    private val webSettings: WebSettings

    init {
        this.webSettings = webView.settings
    }

    private fun isNetworkAvailable(): Boolean {
        val manager = activity.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager
        val networkInfo = manager.activeNetworkInfo

        // Wifi or Mobile Network is present and connected
        return networkInfo != null && networkInfo.isConnected
    }

    // manipulate cache settings to make sure our web app gets updated
    private fun useCache(use: Boolean) = if (use) {
        webSettings.cacheMode = WebSettings.LOAD_CACHE_ELSE_NETWORK
    } else {
        webSettings.cacheMode = WebSettings.LOAD_DEFAULT
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

        if (0 != activity.applicationInfo.flags and ApplicationInfo.FLAG_DEBUGGABLE) {
            WebView.setWebContentsDebuggingEnabled(true)
        }

        webSettings.apply {
            // enable JS
            javaScriptEnabled = true
            // TODO: probably not needed
            // must be set for our js-popup-blocker:
            // setSupportMultipleWindows(true)

            // PWA settings TODO?
            domStorageEnabled = true
            setAppCachePath(activity.applicationContext.cacheDir.absolutePath)

            setAppCacheEnabled(true)
            databaseEnabled = true

            // TODO: probably not needed
            // turn on/off mixed content (both https+http within one page) for API >= 21
//            val enableMixedContent =  false
//            if (enableMixedContent && Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
//                mixedContentMode = WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE
//            }
        }

        // retrieve content from cache primarily if not connected
        forceCacheIfOffline()

        // enable HTML5-support
        webView.webChromeClient = object : WebChromeClient() {
            //simple yet effective redirect/popup blocker
            // TODO? needed???
//            override fun onCreateWindow(view: WebView, isDialog: Boolean, isUserGesture: Boolean, resultMsg: Message): Boolean {
//                val href = view.handler.obtainMessage()
//                view.requestFocusNodeHref(href)
//                val popupUrl = href.data.getString("url")
//                if (popupUrl != null) {
//                    //it's null for most rouge browser hijack ads
//                    webView.loadUrl(popupUrl)
//                    return true
//                }
//                return false
//            }

            // update ProgressBar
            override fun onProgressChanged(view: WebView, newProgress: Int) {
                uiManager.setLoadingProgress(newProgress)
                super.onProgressChanged(view, newProgress)
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

            // TODO! comment out from release
/*            override fun onReceivedSslError(view: WebView, handler: SslErrorHandler, error: SslError) {
                if (Build.FINGERPRINT.contains("generic")) {
                    // Ignore SSL certificate errors in emulator
                    handler.proceed()
                } else {
                    super.onReceivedSslError(view, handler, error)
                }
            }*/
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
            Log.e("sasas", "offline: $errorCode")
            uiManager.setOffline(true)
            // TODO?
        } else {
            // Unsupported Scheme, recover
            Handler().postDelayed({ goBack() }, 100)
        }
    }

    // handle external urls
    private fun handleUrlLoad(view: WebView, url: String): Boolean {
        // prevent loading content that isn't ours
        if (!url.startsWith(activity.appBaseUrl)) {
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
            uiManager.setLoading(true)
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
        webView.loadUrl(activity.appUrl)
    }

    // Lifecycle callbacks
    fun onPause() {
        webView.onPause()
    }

    fun onResume() {
        webView.onResume()
    }
}