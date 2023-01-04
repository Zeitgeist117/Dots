// ==UserScript==
// @name         YouTube - Proper Description
// @namespace    q1k
// @version      1.6.2
// @description  Watch page description below the video with proper open/close toggle, instead of a side bar.
// @author       q1k
// @match        *://www.youtube.com/*
// @grant        none
// @run-at       document-end
// ==/UserScript==
 
function findElement(selector) {
    return new Promise(function(resolve) {
        if (document.querySelector(selector)) {
            return resolve(document.querySelector(selector));
        }
        const observer = new MutationObserver(function(mutations) {
            if (document.querySelector(selector)) {
                resolve(document.querySelector(selector));
                observer.disconnect();
            }
        });
        observer.observe(document, {
            childList: true,
            subtree: true
        });
    });
}
function watchExistingButtonForChange(oldbutton, newbutton, newbuttontext) {
    if(newbutton.innerText.trim().length>0) {
        return;
    }
    newbuttontext.innerText = oldbutton.innerText.replace("...","").trim();
    var mo = new MutationObserver(function(mutations) {
        if(oldbutton.innerText.trim().length>0) {
            newbuttontext.innerText = oldbutton.innerText.replace("...","").trim();
            mo.disconnect();
        }
    });
    mo.observe(oldbutton, {
        childList: true,
        subtree: true,
        characterData: true
    });
}
var more, less, description;
function addButton(open, idname, button_current) {
    let button_new = document.createElement("div");
    //button.setAttribute("id", idname);
    button_new.setAttribute("class","desc_toggles "+idname);
    button_new.innerHTML = "<div class='desc_text more-button style-scope ytd-video-secondary-info-renderer'></div>";
    let button_new_text = button_new.querySelector(".desc_text");
    button_current.parentNode.appendChild(button_new);
    description = button_current.closest("ytd-expander");
    return [button_new, button_new_text];
}
 
var styles = document.createElement("style");
styles.innerHTML=`
ytd-watch-metadata { display: none !important; }
#meta-contents[hidden], #info-contents[hidden]{ display: block !important; }
#meta tp-yt-paper-button#more, #meta tp-yt-paper-button#less, #meta ytd-expander[collapsed] .description_close, #meta ytd-expander:not([collapsed]) .description_open, #meta ytd-expander tp-yt-paper-button.ytd-expander[hidden] ~ tp-yt-paper-button.ytd-expander[hidden] ~ div.desc_toggles { display: none !important; }
#meta .desc_toggles { display: inline-block; cursor: pointer; }
ytd-video-primary-info-renderer[use-yt-sans20-light] .title.ytd-video-primary-info-renderer { line-height: 2.6rem !important; font-weight: 400 !important; font-size: 1.8rem !important; font-family: "Roboto",sans-serif !important; }
#player #cinematics, #player #cinematics canvas { display: none !important; }
/**/
/*game info cards*/
[dark] ytd-rich-metadata-renderer[darker-dark-theme] a { background-color: #242424;/*rgba(255,255,255,0.05;*/ }
/*uploader profile pic*/
#meta #avatar { width: 48px; height: 48px; margin-right: 16px; }
#meta #avatar img { width: 100%; }
/**/
/*general buttons, light and dark theme*/
#subscribe-button ytd-button-renderer a,
#meta ytd-video-owner-renderer ytd-button-renderer a,
#meta ytd-video-owner-renderer ytd-button-renderer button,
#channel-header-container #meta ~ #buttons ytd-button-renderer a,
#channel-header-container #meta ~ #buttons ytd-button-renderer button
{ border-radius: 2px !important; text-transform: uppercase !important; letter-spacing: 0.5px;
 background: /*rgb(7,92,211,0.1)*/ transparent !important; border: 1px solid #075cd3 !important; color: #075cd3 !important; }
 
[dark] #subscribe-button ytd-button-renderer a,
[dark] #meta ytd-video-owner-renderer ytd-button-renderer a,
[dark] #meta ytd-video-owner-renderer ytd-button-renderer button,
[dark] #channel-header-container #meta ~ #buttons ytd-button-renderer a,
[dark] #channel-header-container #meta ~ #buttons ytd-button-renderer button
{ background: /*rgba(62,166,255,0.1)*/ transparent !important; border: 1px solid #3ea6ff !important; color: #3ea6ff !important; }
 
/*
light{ background: rgba(0,0,0,0.1) !important; border: none !important; color: #000 !important; }
dark{ background: rgba(255,255,255,0.1) !important; border: none !important; color: #aaa !important; }
*/
 
/*owner buttons*/
#edit-buttons ytd-button-renderer a,
#edit-buttons ytd-button-renderer button
{ letter-spacing: 0.5px; background: rgb(7,92,211,0.1) !important; border: 1px solid #075cd3 !important; color: #075cd3 !important; }
[dark] #edit-buttons ytd-button-renderer a,
[dark] #edit-buttons ytd-button-renderer button
{ background: rgba(62,166,255,0.1) !important; border: 1px solid #3ea6ff !important; color: #3ea6ff !important; }
#sponsor-button ytd-button-renderer button { border-radius: 2px !important; text-transform: uppercase !important; }
 
/*sub button*/
#subscribe-button ytd-subscribe-button-renderer button.yt-spec-button-shape-next--filled { background: #c00 !important; color: #fff; }
#subscribe-button ytd-subscribe-button-renderer button.yt-spec-button-shape-next--tonal { background: rgba(0,0,0,0.1) !important; color: #000; }
[dark] #subscribe-button ytd-subscribe-button-renderer button.yt-spec-button-shape-next--tonal { background: rgba(255,255,255,0.1) !important; color: #aaa; }
#subscribe-button ytd-subscribe-button-renderer button { letter-spacing: 0.5px; border-radius: 2px !important; text-transform: uppercase !important; }
 
/*join/joined button */
#sponsor-button ytd-button-renderer button.yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal
{ background: /*rgb(7,92,211,0.1)*/ transparent !important; border: 1px solid #075cd3 !important; color: #075cd3 !important; }
[dark] #sponsor-button ytd-button-renderer button.yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal
{ background: /*rgba(62,166,255,0.1)*/ transparent !important; border: 1px solid #3ea6ff !important; color: #3ea6ff !important; }
 
#meta ytd-video-owner-renderer #sponsor-button ytd-button-renderer a.yt-spec-button-shape-next--tonal,
#channel-header-container #meta ~ #buttons #sponsor-button ytd-button-renderer a.yt-spec-button-shape-next--tonal
{ background: rgba(0,0,0,0.1) !important; color: #000 !important; border: none !important; }
 
[dark] #meta ytd-video-owner-renderer #sponsor-button ytd-button-renderer a.yt-spec-button-shape-next--tonal,
[dark] #channel-header-container #meta ~ #buttons #sponsor-button ytd-button-renderer a.yt-spec-button-shape-next--tonal
{ background: rgba(255,255,255,0.1) !important; color: #aaa !important; border: none !important; }
 
 
/**/
 
/**/
#info .yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal { background: transparent !important; }
#meta .yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal:hover, #info .yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal:hover { /*background: rgba(0,0,0,0.1) !important;*/ }
[dark] #meta .yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal:hover, [dark] #info .yt-spec-button-shape-next--mono.yt-spec-button-shape-next--tonal:hover { /*background: rgba(255,255,255,0.1) !important;*/ }
/**/
#info ytd-toggle-button-renderer yt-button-shape button, ytd-button-renderer yt-button-shape button { width: auto; border-radius: 0 !important; }
#info ytd-toggle-button-renderer tp-yt-paper-tooltip #tooltip, #info ytd-button-renderer tp-yt-paper-tooltip #tooltip { width: max-content; }
#info #top-level-buttons-computed > ytd-button-renderer button { padding: 0 8px; text-transform: uppercase; }
#info #flexible-item-buttons > * { margin-left: 8px;}
#info #flexible-item-buttons ytd-button-renderer button { padding: 0 8px; text-transform: uppercase; }
ytd-segmented-like-dislike-button-renderer button { padding: 0 10px 0 12px !important; }
/*fix for Return Youtube Dislikes*/
ytd-segmented-like-dislike-button-renderer button span#text { margin-left: 6px; }
#info ytd-menu-renderer[has-flexible-items] { /*overflow:unset !important;*/ }
#info ytd-segmented-like-dislike-button-renderer { position: relative; }
#info .ryd-tooltip { position: absolute; bottom: 0px; margin: 0 !important; top: auto; width: 100% !important; display: none; }
#info ytd-segmented-like-dislike-button-renderer .ryd-tooltip { display: block; }
#info #ryd-bar-container { width: 100% !important; }
#info #ryd-dislike-tooltip { top: -40px !important; pointer-events: none; }
/**/
ytd-comments#comments #replies #expander .more-button button,
ytd-comments#comments #replies #expander .less-button button { background: transparent !important; }
ytd-comments#comments #replies #expander .more-button button yt-touch-feedback-shape,
ytd-comments#comments #replies #expander .less-button button yt-touch-feedback-shape { display: none !important; }
/*remove like animation*/
ytd-segmented-like-dislike-button-renderer #segmented-like-button button lottie-component svg > g > g[clip-path*="url(#__lottie_element_"] { transform: matrix(1.0880000591278076,0,0,1.0880000591278076,69.95299530029297,67.9433822631836) !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button lottie-component svg g g g { transform: matrix(1,0,0,1,60,60) !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button lottie-component svg > g > g[clip-path*="url(#__lottie_element_"]:first-child { display: none !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button lottie-component svg g path { fill: #000 !important; stroke: #000; }
[dark] ytd-segmented-like-dislike-button-renderer #segmented-like-button button lottie-component svg g path { fill: #fff !important; stroke: #fff; }
 
ytd-segmented-like-dislike-button-renderer #segmented-like-button button svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(1) path { d:path(" M25.025999069213867,-4.00600004196167 C25.025999069213867,-4.00600004196167 5.992000102996826,-3.996999979019165 5.992000102996826,-3.996999979019165 C5.992000102996826,-3.996999979019165 11.012999534606934,-22.983999252319336 11.012999534606934,-22.983999252319336 C12.230999946594238,-26.90399932861328 13,-31.94300079345703 8.994000434875488,-31.981000900268555 C7,-32 5,-32 4.021999835968018,-31.007999420166016 C4.021999835968018,-31.007999420166016 -19.993000030517578,-5.03000020980835 -19.993000030517578,-5.03000020980835 C-19.993000030517578,-5.03000020980835 -20.027999877929688,32.025001525878906 -20.027999877929688,32.025001525878906 C-20.027999877929688,32.025001525878906 20.97599983215332,31.986000061035156 20.97599983215332,31.986000061035156 C25.010000228881836,31.986000061035156 26.198999404907227,29.562000274658203 26.99799919128418,25.985000610351562 C26.99799919128418,25.985000610351562 31.972000122070312,4.026000022888184 31.972000122070312,4.026000022888184 C33,-0.6930000185966492 30.392000198364258,-4.00600004196167 25.025999069213867,-4.00600004196167z") !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(3) path { d:path(" M-27.993000030517578,-4.015999794006348 C-27.993000030517578,-4.015999794006348 -36.02799987792969,-3.996999979019165 -36.02799987792969,-3.996999979019165 C-36.02799987792969,-3.996999979019165 -36,31.9950008392334 -36,31.9950008392334 C-36,31.9950008392334 -28.027999877929688,31.976999282836914 -28.027999877929688,31.976999282836914 C-28.027999877929688,31.976999282836914 -27.993000030517578,-4.015999794006348 -27.993000030517578,-4.015999794006348z") !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="false"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(1),
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="false"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(3) { display: none !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="true"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(1),
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="true"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(3) { display: block !important; }
 
ytd-segmented-like-dislike-button-renderer #segmented-like-button button svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(2) path { d:path(" M25.025999069213867,-4.00600004196167 C25.025999069213867,-4.00600004196167 5.992000102996826,-3.996999979019165 5.992000102996826,-3.996999979019165 C5.992000102996826,-3.996999979019165 11.012999534606934,-22.983999252319336 11.012999534606934,-22.983999252319336 C12.230999946594238,-26.90399932861328 13,-31.94300079345703 8.994000434875488,-31.981000900268555 C7,-32 5,-32 4.021999835968018,-31.007999420166016 C4.021999835968018,-31.007999420166016 -19.993000030517578,-5.03000020980835 -19.993000030517578,-5.03000020980835 C-19.993000030517578,-5.03000020980835 -20.027999877929688,32.025001525878906 -20.027999877929688,32.025001525878906 C-20.027999877929688,32.025001525878906 20.97599983215332,31.986000061035156 20.97599983215332,31.986000061035156 C25.010000228881836,31.986000061035156 26.198999404907227,29.562000274658203 26.99799919128418,25.985000610351562 C26.99799919128418,25.985000610351562 31.972000122070312,4.026000022888184 31.972000122070312,4.026000022888184 C33,-0.6930000185966492 30.392000198364258,-4.00600004196167 25.025999069213867,-4.00600004196167z") !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(4) path { d:path(" M-19.986000061035156,-4.03000020980835 C-19.986000061035156,-4.03000020980835 -36.020999908447266,-3.996999979019165 -36.020999908447266,-3.996999979019165 C-36.020999908447266,-3.996999979019165 -36.00199890136719,31.993000030517578 -36.00199890136719,31.993000030517578 C-36.00199890136719,31.993000030517578 -20.030000686645508,32.02299880981445 -20.030000686645508,32.02299880981445 C-20.030000686645508,32.02299880981445 -19.986000061035156,-4.03000020980835 -19.986000061035156,-4.03000020980835z") !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="false"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(2),
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="false"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(4) { display: block !important; }
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="true"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(2),
ytd-segmented-like-dislike-button-renderer #segmented-like-button button[aria-pressed="true"] svg g g[clip-path*="url(#__lottie_element_"] > :nth-child(4) { display: none !important; }
 
`;
/*
var styles_alt = document.createElement("style");
    styles_alt.innerHTML=`
#meta ytd-video-secondary-info-renderer, #primary #meta .desc_toggles { border-color: rgba(0,0,0,0.125); padding-bottom: 0; border-bottom: none; }
[dark] #meta ytd-video-secondary-info-renderer, [dark] #primary #meta .desc_toggles { border-color: rgba(255,255,255,0.125); }
 
#meta ytd-expander.ytd-video-secondary-info-renderer, #meta ytd-expander.ytd-video-secondary-info-renderer > * { margin-left: 0; }
#meta .desc_toggles { width: 100%; border-top: 1px solid; border-radius: 0; text-align: center; cursor: pointer; margin-top: 1em; background: linear-gradient(rgba(0,0,0,0.02), transparent); }
[dark] #meta .desc_toggles { background: linear-gradient(rgba(255,255,255,0.02), transparent); }
#meta .desc_toggles:hover { background: rgba(0,0,0,0.03); }
[dark] #meta .desc_toggles:hover { background: rgba(255,255,255,0.03); }
#meta .desc_toggles .desc_text { margin: 0; padding: 4px; }
`;*/
document.head.appendChild(styles);
 
findElement("#meta ytd-video-secondary-info-renderer ytd-expander tp-yt-paper-button#more").then(function(el){
    more = el;
    let buttons = addButton(true, "description_open", el);
    buttons[0].addEventListener('click', function(e) {
        description.removeAttribute("collapsed");
        more.setAttribute("hidden","");
        less.removeAttribute("hidden");
    });
    if(typeof yt !== "undefined")
        yt.msgs_.SHOW_MORE ? buttons[1].innerText=yt.msgs_.SHOW_MORE : buttons[1].innerText = more.innerText.replace("...","").trim();
    watchExistingButtonForChange(el, buttons[0], buttons[1]);
});
findElement("#meta ytd-video-secondary-info-renderer ytd-expander tp-yt-paper-button#less").then(function(el){
    less = el;
    let buttons = addButton(false, "description_close", el);
    buttons[0].addEventListener('click', function(e) {
        description.setAttribute("collapsed","");
        less.setAttribute("hidden","");
        more.removeAttribute("hidden");
    });
    if(typeof yt !== "undefined")
        yt.msgs_.SHOW_LESS ? buttons[1].innerText=yt.msgs_.SHOW_LESS : buttons[1].innerText = less.innerText.replace("...","").trim();
    watchExistingButtonForChange(el, buttons[0], buttons[1]);
});
findElement("#info ytd-video-primary-info-renderer").then(function(el){
    el.removeAttribute("use-yt-sans20-light");
});
findElement("ytd-watch-metadata").then(function(el){ //fix livestream viewers counter
    setTimeout(function(){
        el.remove();
    },20000);
    /*document.addEventListener('yt-page-data-updated', function(event) {
        el.remove();
    }, {once: true});*/
});
 
findElement(".ryd-tooltip").then(function(el){
    //if(Boolean(el.closest("ytd-watch-metadata"))){
        findElement("#info ytd-segmented-like-dislike-button-renderer").then(function(el1){
            el1.appendChild(el);
        });
    //}
});
