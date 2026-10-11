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


  // Function reference pages: the group of the function, the other functions of the group,
  // and the previous and next function, in the order of the reference index
  var REF_TEXT = {
    en: { index: "Function reference", all: "All functions", previous: "Previous", next: "Next",
          group: "In this group", of: " of " },
    zh: { index: "函数参考", all: "全部函数", previous: "上一个", next: "下一个",
          group: "同组函数", of: " / " }
  };
  var GROUPS_ZH = {
    "💾 FAO data": "💾 FAO 数据",
    "🌊 Fishing areas": "🌊 渔区分析",
    "🐟 Species and countries": "🐟 物种与国家",
    "📊 Summary analyses": "📊 汇总分析",
    "📝 Reports": "📝 报告",
    "🎨 Settings and language": "🎨 设置与语言"
  };

  function element(tag, className, text) {
    var node = document.createElement(tag);
    if (className) node.className = className;
    if (text) node.textContent = text;
    return node;
  }

  function referenceNav(root, lang) {
    var rel = relative(window.location.href, root);
    var match = rel && rel.match(/^reference\/([^\/]+\.html)$/);
    if (!match || match[1] === "index.html" || !window.fetch || !window.DOMParser) return;
    var current = match[1];
    var text = REF_TEXT[lang] || REF_TEXT.en;
    var groupName = function (name) { return lang === "zh" && GROUPS_ZH[name] ? GROUPS_ZH[name] : name; };

    fetch(root + "reference/index.html")
      .then(function (response) { return response.ok ? response.text() : Promise.reject(response.status); })
      .then(function (html) {
        var doc = new DOMParser().parseFromString(html, "text/html");
        var topics = [];
        var group = "";
        doc.querySelectorAll("main h2, main dt a[href]").forEach(function (node) {
          if (node.tagName === "H2") { group = node.textContent.trim(); return; }
          var href = node.getAttribute("href");
          if (!/^[^\/#]+\.html$/.test(href) || topics.some(function (t) { return t.href === href; })) return;
          topics.push({ href: href, name: node.textContent.trim(), group: group });
        });
        var position = topics.map(function (t) { return t.href; }).indexOf(current);
        var main = document.querySelector("main");
        if (!main) return;

        // Group and position above the title
        var header = main.querySelector(".page-header");
        var crumb = element("div", "fao-breadcrumb");
        var toIndex = element("a", null, text.index);
        toIndex.href = "index.html";
        crumb.appendChild(toIndex);
        if (position >= 0) {
          var mine = topics[position];
          var inGroup = topics.filter(function (t) { return t.group === mine.group; });
          crumb.appendChild(element("span", "fao-breadcrumb-sep", "›"));
          crumb.appendChild(element("span", null, groupName(mine.group) + " · " +
            (inGroup.indexOf(mine) + 1) + text.of + inGroup.length));
        }
        if (header) header.insertBefore(crumb, header.querySelector("h1"));
        if (position < 0) return;

        // Previous and next function at the end of the page
        var pager = element("nav", "fao-pager");
        pager.setAttribute("aria-label", text.index);
        var card = function (topic, label, className) {
          if (!topic) return element("span", className + " fao-pager-empty");
          var link = element("a", className);
          link.href = topic.href;
          link.appendChild(element("span", "fao-pager-label", label));
          link.appendChild(element("code", null, topic.name));
          link.appendChild(element("span", "fao-pager-group", groupName(topic.group)));
          return link;
        };
        pager.appendChild(card(topics[position - 1], "← " + text.previous, "fao-pager-prev"));
        var all = element("a", "fao-pager-index", "☰ " + text.all);
        all.href = "index.html";
        pager.appendChild(all);
        pager.appendChild(card(topics[position + 1], text.next + " →", "fao-pager-next"));
        main.appendChild(pager);

        // Other functions of the group in the sidebar, below "On this page"
        var aside = document.querySelector("aside");
        if (aside) {
          var box = element("nav", "fao-group-nav");
          box.setAttribute("aria-label", text.group);
          box.appendChild(element("h2", null, text.group + " · " + groupName(topics[position].group)));
          var list = element("ul");
          topics.filter(function (t) { return t.group === topics[position].group; }).forEach(function (t) {
            var item = element("li", t.href === current ? "active" : null);
            var link = element("a", null, t.name);
            link.href = t.href;
            if (t.href === current) link.setAttribute("aria-current", "page");
            item.appendChild(link);
            list.appendChild(item);
          });
          box.appendChild(list);
          // Keep the group list in view with "On this page", which pkgdown makes sticky
          var holder = element("div", "fao-aside-sticky");
          var toc = aside.querySelector("#toc");
          if (toc) {
            toc.parentNode.insertBefore(holder, toc);
            holder.appendChild(toc);
          } else {
            aside.appendChild(holder);
          }
          holder.appendChild(box);
        }
      })
      .catch(function () { /* reference index not available, e.g. a page opened from disk */ });
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

    referenceNav(root, lang);

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
