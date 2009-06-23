(in-package "COMMON-LISP-USER")

;;; in the tradition of forth

(defvar *pad*)

(defun get-page (url &key parameters)
  (setf *pad*
        (drakma:http-request url :parameters parameters)))

(defun pluck (pattern)
  (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings pattern *pad*)
    (declare (ignore match))
    (if regs
        (svref regs 0)
        nil)))



;;; List gleaned from
;;; http://www.google.com/codesearch/p?&cd=1&ct=rc#iAVVwEcJc_8/trunk/firefox/ghostery-statusbar/ghostery/chrome/content/ghostery-db.js


(defvar *bugs* '("Lookery"
                 "Google Analytics"
                 "MyBlogLog"
                 "Quantcast"
                 "IndexTools"
                 "SiteMeter"
                 "Lijit"
                 "Omniture"
                 "Crazy Egg"
                 "Snap"
                 "Statcounter"
                 "Piwik Analytics"
                 "Mint"
                 "Facebook Beacon"
                 "Typepad Stats"
                 "Wordpress Stats"
                 "HubSpot"
                 "Yahoo Analytics"
                 "OrangeSoda"
                 "Engagd"
                 "Nugg.Ad"
                 "Crowd Science"
                 "Federated Media"
                 "OpenAds"
                 "Amazon Associates"
                 "FeedBurner"
                 "ClustrMaps"
                 "Feedjit"
                 "Google Adsense"
                 "HitTail"
                 "FriendFeed"
                 "Woopra"
                 "ScribeFire QuickAds"
                 "NetRatings SiteCensus"
                 "Doubleclick"
                 "Tacoda"
                 "RightMedia"
                 "Dynamic Logic"
                 "WebTrends"
                 "XiTi"
                 "ShareThis"
                 "Seesmic"
                 "AddtoAny"
                 "AddThis"
                 "Revenue Science"
                 "PointRoll"
                 "ChartBeat"
                 "Clicky"
                 "UserVoice"
                 "Rubicon"
                 "ConversionRuler"
                 "Salesforce"
                 "Sphere"
                 "Criteo"
                 "Cubics"
                 "InfoLinks"
                 "Statisfy"
                 "MSN Ads"
                 "Outbrain"
                 "Google FriendConnect"
                 "SpecificClick"
                 "Microsoft Atlas"
                 "Skribit"
                 "Google Custom Search Engine"
                 "Google AJAX Search API"
                 "Kontera ContentLink"
                 "AdBrite"
                 "AdultAdWorld"
                 "Gunggo"
                 "DoublePimp"
                 "SexInYourCity"
                 "Clicksor"
                 "HubSpot WebsiteGrader"
                 "Quigo AdSonar"
                 "BlogCatalog"
                 "Technorati Widget"
                 "Alexa Traffic Rank"
                 "Tribal Fusion"
                 "Disqus"
                 "Six Apart Advertising"
                 "BlogHer Ads"
                 "Advertising.com"
                 "LeadBack"
                 "DiggThis"
                 "DoubleClick Spotlight"
                 "Yahoo Overture"
                 "Intense Debate"
                 "Facebook Connect"
                 "BTBuckets"
                 "gumgum"
                 "YieldBuild"
                 "Yahoo Buzz"
                 "Baynote Observer"
                 "TriggIt"
                 "Digg Widget"
                 "Blogads"
                 "Zedo"
                 "Vibrant Ads"
                 "GetSatisfaction"
                 "Adify"
                 "Google Widgets"
                 "LivePerson"
                 "Kampyle"
                 "ClickTale"
                 "Lotame"
                 "CPX Interactive"
                 "Lynchpin Analytics"
                 "Trovus Revelations"
                 "Omniture TouchClarity"
                 "InsightExpress"
                 "Kanoodle"
                 "BlueKai"
                 "Loomia"
                 "Others Online"
                 "TwitterCounter"
                 "Thummit"
                 "Dotomi"
                 "Chitika"
                 "Spot200"
                 "HitsLink"
                 "W3Counter"
                 "AWStats"
                 "OneStat"
                 "Twitter Badge"
                 "DuckDuckGo"
                 "Bluemetrix"
                 "Reinvigorate"
                 "PostRank"
                 "Collarity"
                 "AdaptiveBlue SmartLinks"
                 "Tumblr"
                 "BlogRollr"
                 "Casale Media"
                 "BlogCounter"
                 "WidgetBucks"
                 "Nooked"
                 "ValueClick Mediaplex"
                 "JS-Kit"
                 "Bzzster"
                 "LeadLander"
                 "Burst Media"
                 "Zango"
                 "Adknowledge"
                 "NebuAd"
                 "Media6 Degrees"
                 "FunctionalTrends"
                 "Nuconomy"
                 "Bluelithium"
                 "Glam Media"
                 "Trafic"
                 "Lyris ClickTracks"
                 "Enquisite"
                 "eXTReMe Tracker"
                 "Microsoft Analytics"
                 "Sweepery"
                 "Tell-a-Friend"
                 "PercentMobile"
                 "NetMonitor"
                 "Marketo"
                 "Demandbase"
                 "FetchBack"
                 "SilverPop"
                 "CoreMetrics"
                 "Magnify360"
                 "Fathom SEO"
                 "Eloqua"
                 "Acerno"
                 "Mindset Media"
                 "GoDaddy Site Analytics"
                 "AdNexus"
                 "AlmondNet"
                 "Collective Media"
                 "eXelate"
                 "Fox Audience Network"
                 "interCLICK"
                 "NextAction"
                 "Traffic Marketplace"
                 "Turn"
                 "Real Media"
                 "etracker"
                 "Comscore VoiceFive"
                 "Bizo"
                 "Snoobi"
                 "Rocket Fuel"
                 "ShinyStat"
                 "VisiStat"
                 "NedStat"
                 "Tynt Tracer"
                 "i-stats"))


;;; The actual scrapping

(defun scrape-tracker-page (url)
  "Pluck junk out of pages like http://www.ghostery.com/apps/chartbeat."
  (get-page url)
  (format t "~&~4A ~40A ~20A ~A"
          (let ((n (pluck "found on.*<b>([\\d,]+)</b>")))
            (when n
              (parse-integer (delete #\, n))))
          (pluck "Website: <a rel=\"nofollow\" href=\"([^\"]*)\"")
          (pluck "<h1>([^<]*)</h1>")
          url))

(defun scrape-trackers-via-google ()
  "Use google to find apps pages on ghostery.com, then scrape em."
  (loop
    with u = "http://www.google.com/search?q=site:ghostery.com+%22Application+Owner%22&hl=en&sa=N"
    for i in '("0" "10" "20" "30" "40")
    as p = (get-page u :parameters `(("start" . ,i)))
    do
 (cl-ppcre:do-register-groups (link)
     ("href=\"(http://www.ghostery.com/apps/[^\"]*)\"" p)
   (scrape-tracker-page link))))

(defun scrape-trackers-via-bugs ()
  (loop for bug in *bugs*
        as url = (concatenate 'string "http://www.ghostery.com/apps/"
                              (nstring-downcase
                               (substitute #\_ #\space bug)))
        do (scrape-tracker-page url)))
