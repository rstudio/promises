
const htmlContainer = document.getElementsByTagName("html")[0];

// Function to update the image source based on the theme attribute
const updateImageSrc = (newTheme) => {
  const logo = document.getElementsByClassName("logo")[0];
  console.log("Updating image source for theme:", logo, newTheme);
  if (newTheme === 'dark') {
    logo.src = 'reference/figures/logo-dark.svg';
  } else {
    logo.src = 'logo.svg';
  }
};

// Create a new MutationObserver
const observer = new MutationObserver((mutations) => {
  console.log("Mutations!", mutations);
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
