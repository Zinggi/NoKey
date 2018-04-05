package ch.ethz.nokey.nokey

import android.os.Build
import android.view.View
import android.view.animation.AccelerateInterpolator
import android.view.animation.DecelerateInterpolator
import android.webkit.WebView
import android.widget.LinearLayout
import android.widget.ProgressBar


class UIManager(activity: MainActivity) {
    private val webView: WebView = activity.findViewById<View>(R.id.webView) as WebView
    private val progressSpinner: ProgressBar = activity.findViewById<View>(R.id.progressSpinner) as ProgressBar
    private val progressBar: ProgressBar = activity.findViewById<View>(R.id.progressBarBottom) as ProgressBar
    private val offlineContainer: LinearLayout = activity.findViewById<View>(R.id.offlineContainer) as LinearLayout
    private var pageLoaded = false
    // show your app when the page is loaded XX %.
    // lower it, if you've got server-side rendering (e.g. to 35),
    // bump it up to ~98 if you don't have SSR or a loading screen in your web app
    private val progressThreshold = 98
    // window transition duration in ms
    private val transitionDuration = 2200.0f

    init {
        // set click listener for offline-screen
        offlineContainer.setOnClickListener {
            webView.loadUrl(activity.appUrl)
            setOffline(false)
        }
    }

    // Set Loading Progress for ProgressBar
    fun setLoadingProgress(progress: Int) {
        // set progress in UI
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            progressBar.setProgress(progress, true)
        } else {
            progressBar.progress = progress
        }

        // hide ProgressBar if not applicable
        if (progress in 0..99) {
            progressBar.visibility = View.VISIBLE
        } else {
            progressBar.visibility = View.INVISIBLE
        }

        // get app screen back if loading is almost complete
        if (progress >= progressThreshold && !pageLoaded) {
            setLoading(false)
        }
    }

    // Show loading animation screen while app is loading/caching the first time
    fun setLoading(isLoading: Boolean) {
        if (isLoading) {
            progressSpinner.visibility = View.VISIBLE
            webView.animate().translationX(transitionDuration)
                    .alpha(0.5f).setInterpolator(AccelerateInterpolator()).start()

        } else {
            webView.translationX = transitionDuration * -1
            webView.animate().translationX(0f).alpha(1f).setInterpolator(DecelerateInterpolator()).start()
            progressSpinner.visibility = View.INVISIBLE
        }
        pageLoaded = !isLoading
    }

    // handle visibility of offline screen
    fun setOffline(offline: Boolean) {
        if (offline) {
            setLoadingProgress(100)
            webView.visibility = View.INVISIBLE
            offlineContainer.visibility = View.VISIBLE
        } else {
            webView.visibility = View.VISIBLE
            offlineContainer.visibility = View.INVISIBLE
        }
    }

    // set icon in recent activity view to a white one to be visible in the app bar
    // TODO: needed?
//    fun changeRecentAppsIcon() {
//        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
//            val iconWhite = BitmapFactory.decodeResource(activity.resources, R.drawable.ic_appbar)
//
//            val typedValue = TypedValue()
//            val theme = activity.theme
//            theme.resolveAttribute(R.attr.colorPrimary, typedValue, true)
//            val color = typedValue.data
//
//            val description = ActivityManager.TaskDescription(
//                    activity.resources.getString(R.string.app_name),
//                    iconWhite,
//                    color
//            )
//            activity.setTaskDescription(description)
//            iconWhite.recycle()
//        }
//    }
}