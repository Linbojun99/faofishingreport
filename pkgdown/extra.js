// Language switch of the website.
// English pages live at <site>/..., their Chinese versions at <site>/articles/zh/...
// The "中文 / English" button in the navbar opens the other version of the page. The choice is
// remembered, so the navbar links lead to pages in the chosen language. Function reference
// and news pages exist in English only; in Chinese mode their navbar is shown in Chinese.
(function () {
  var STORAGE_KEY = "faofishingreport-lang";

  // Navbar and page text, English -> Chinese
  var DICT = {
    "🚀 Get started": "🚀 快速入门",
    "Reference": "函数参考",
    "Articles": "文章",
    "News": "更新日志",
    "Changelog": "更新日志",
    "💾 FAO data: download and customise": "💾 FAO 数据：下载与自定义",
    "🌊 Fishing area analysis": "🌊 渔区分析",
    "🐟 Species and countries": "🐟 物种与国家分析",
    "📈 Composition, stock status and trends": "📈 类群结构、资源状态与趋势",
    "🔍 Data quality, diversity and fleets": "🔍 数据质量、多样性与船队",
    "🌡️ Sea temperature and catch temperature": "🌡️ 海表温度与渔获平均温度",
    "🎨 Customise charts and maps": "🎨 自定义图表与地图",
    "On this page": "本页目录",
    "Developed by": "开发者",
    "Site built with": "网站由"
  };

  function readStored() {
    try { return window.localStorage.getItem(STORAGE_KEY); } catch (e) { return null; }
  }
  function store(lang) {
    try { window.localStorage.setItem(STORAGE_KEY, lang); } catch (e) { /* storage blocked */ }
  }

  // Root URL of the site, from the package name link in the navbar
  function siteRoot() {
    var brand = document.querySelector("a.navbar-brand");
    var href = brand ? brand.getAttribute("href") : "index.html";
    return new URL(href.replace(/index\.html$/, "") || "./", window.location.href).href;
  }

  // Path of a URL relative to the site root ("" for the home page)
  function relative(url, root) {
    var clean = url.split("#")[0].split("?")[0];
    return clean.indexOf(root) === 0 ? clean.slice(root.length) : null;
  }

  // The Chinese (toZh = true) or English version of a page, or null if it has none
  function counterpart(rel, toZh) {
    if (rel === null) return null;
    if (toZh) {
      if (rel === "" || rel === "index.html" || rel === "articles/index.html") return "articles/zh/index.html";
      var en = rel.match(/^articles\/([^\/]+\.html)$/);
      return en ? "articles/zh/" + en[1] : null;
    }
    var zh = rel.match(/^articles\/zh\/([^\/]+\.html)$/);
    if (!zh) return null;
    return zh[1] === "index.html" ? "index.html" : "articles/" + zh[1];
  }

  function translateText(root) {
    var nodes = document.querySelectorAll(
      ".navbar a.nav-link, .navbar a.dropdown-item, .navbar .dropdown-toggle, #toc h2, nav#toc > h2, footer p, footer div"
    );
    nodes.forEach(function (node) {
      node.childNodes.forEach(function (child) {
        if (child.nodeType !== Node.TEXT_NODE) return;
        var text = child.textContent.trim();
        if (DICT[text]) {
          child.textContent = child.textContent.replace(text, DICT[text]);
        } else {
          Object.keys(DICT).forEach(function (key) {
            if (text.indexOf(key) === 0 && key.length > 8) {
              child.textContent = child.textContent.replace(key, DICT[key]);
            }
          });
        }
      });
    });
    var search = document.querySelector("input#search-input");
    if (search) search.setAttribute("placeholder", "搜索");
  }

  // In Chinese mode, navbar links to English articles point to their Chinese versions
  function rewriteLinks(root) {
    document.querySelectorAll(".navbar a.nav-link, .navbar a.dropdown-item, .navbar a.navbar-brand").forEach(function (link) {
      var href = link.getAttribute("href");
      if (link.classList.contains("fao-lang-button")) return;
      if (!href || href.charAt(0) === "#" || /^https?:/.test(href)) return;
      var target = counterpart(relative(new URL(href, window.location.href).href, root), true);
      if (target) link.setAttribute("href", root + target);
    });
  }

  function englishOnlyNote() {
    var main = document.querySelector("main");
    if (!main || document.querySelector(".fao-lang-note")) return;
    var note = document.createElement("div");
    note.className = "fao-lang-note";
    note.textContent = "本页（函数说明或更新日志）只有英文版。各函数的中文用法与示例请见“文章”菜单中的中文文档。";
    main.insertBefore(note, main.firstChild);
  }

  document.addEventListener("DOMContentLoaded", function () {
    var root = siteRoot();
    var rel = relative(window.location.href, root);
    var onZhPage = rel !== null && rel.indexOf("articles/zh/") === 0;
    var hasZhVersion = counterpart(rel, true) !== null;
    var stored = readStored();
    // pkgdown writes the "#lang" link of the button relative to the page, e.g. "../#lang"
    var buttons = document.querySelectorAll('a.nav-link[href$="#lang"]');
    buttons.forEach(function (button) { button.classList.add("fao-lang-button"); });

    // Keep the chosen language when an English page with a Chinese version is opened
    if (!onZhPage && hasZhVersion && stored === "zh") {
      window.location.replace(root + counterpart(rel, true) + window.location.hash);
      return;
    }

    var lang = onZhPage ? "zh" : (hasZhVersion ? "en" : (stored === "zh" ? "zh" : "en"));
    if (onZhPage) store("zh");
    document.documentElement.setAttribute("lang", lang === "zh" ? "zh-CN" : "en");

    if (lang === "zh") {
      translateText(root);
      rewriteLinks(root);
      if (!onZhPage) englishOnlyNote();
    }

    buttons.forEach(function (button) {
      button.textContent = lang === "zh" ? "English" : "中文";
      button.setAttribute("title", lang === "zh" ? "Switch to English" : "切换到中文");
      button.addEventListener("click", function (event) {
        event.preventDefault();
        var next = lang === "zh" ? "en" : "zh";
        store(next);
        var target = counterpart(rel, next === "zh");
        if (target) {
          window.location.href = root + target;
        } else {
          window.location.reload();
        }
      });
    });
  });
})();
