var sections = document.querySelectorAll("section");
var bubble = document.querySelector(".bubble");

var options = {
  threshold: 0,
};

let observer = new IntersectionObserver(navCheck, options);

function navCheck(entries) {
  entries.forEach((entry) => {
    var className = entry.target.className;
    var activeAnchor = document.querySelector(`[data-page=${className}]`);
    var coords = activeAnchor.getBoundingClientRect();

    var directions = {
      height: coords.height,
      width: coords.width,
      top: coords.top,
      left: coords.left,
    };

    if (entry.isIntersecting) {
      bubble.style.setProperty("left", `${directions.left}px`);
      bubble.style.setProperty("top", `${directions.top}px`);
      bubble.style.setProperty("width", `${directions.width}px`);
      bubble.style.setProperty("height", `${directions.height}px`);
    }
  });
}

sections.forEach((section) => {
  observer.observe(section);
});
