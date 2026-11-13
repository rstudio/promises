
const htmlContainer = document.getElementsByTagName("html")[0];

// pkgdown writes the logo <img> src relative to each page (e.g. "logo.svg"
// at the root, "../logo.svg" under reference/, "../../logo.svg" under
// articles/). Capture that directory prefix once, before we rewrite src, so
// the light/dark swap resolves correctly on every page. Note that pkgdown
// only copies the main logo to the site root; logo-dark.svg lives in
// reference/figures/ alongside the rest of man/figures/.
let logoPrefix = null;
const getLogoPrefix = (logo) => {
  if (logoPrefix === null) {
    const src = logo.getAttribute("src") || "";
    logoPrefix = src.slice(0, src.lastIndexOf("/") + 1);
  }
  return logoPrefix;
};

// Function to update the image source based on the theme attribute
const updateImageSrc = (newTheme) => {
  const logo = document.getElementsByClassName("logo")[0];
  if (!logo) return;
  const prefix = getLogoPrefix(logo);
  if (newTheme === 'dark') {
    logo.src = prefix + 'reference/figures/logo-dark.svg';
  } else {
    logo.src = prefix + 'logo.svg';
  }
};

// Create a new MutationObserver
const observer = new MutationObserver((mutations) => {
  mutations.forEach((mutation) => {
    const newTheme = mutation.target.getAttribute('data-bs-theme');
    updateImageSrc(newTheme);
  });
});

// Start observing the target element for attribute changes
observer.observe(htmlContainer, {
  attributes: true, // Watch for attribute changes
  attributeFilter: ['data-bs-theme'] // Only observe the 'data-bs-theme' attribute
});

const docReady = (fn) => {
    // see if DOM is already available
    if (document.readyState === "complete" || document.readyState === "interactive") {
        // call on next available tick
        setTimeout(fn, 1);
    } else {
        document.addEventListener("DOMContentLoaded", fn);
    }
}
docReady(() => {
    updateImageSrc(htmlContainer.getAttribute('data-bs-theme'));
});
