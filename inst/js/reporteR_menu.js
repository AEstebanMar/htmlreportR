document.addEventListener("DOMContentLoaded", function() {
  // Add click event listeners to menu links
  var menuLinks = document.querySelectorAll('.navbar a');
  menuLinks.forEach(function(link) {
    link.addEventListener('click', function(event) {
      // Prevent default link behavior
      event.preventDefault();
      
      // Get the target section
      var targetId = link.getAttribute('href').substring(1);
      var targetSection = document.getElementById(targetId);
      
      // Scroll to the target section
      window.scrollTo({
        top: targetSection.offsetTop - 100, // Adjusted to account for fixed menu height
        behavior: 'smooth'
      });
    });
  });
});
