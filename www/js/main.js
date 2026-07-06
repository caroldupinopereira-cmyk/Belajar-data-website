/* ============================================================
   Intelligent Data Analytics Dashboard - JavaScript
   ============================================================ */

document.addEventListener('DOMContentLoaded', function() {
  // Navigation
  const navLinks = document.querySelectorAll('.nav-link');
  const sections = document.querySelectorAll('.section');

  navLinks.forEach(link => {
    link.addEventListener('click', function(e) {
      e.preventDefault();
      const target = this.getAttribute('href').substring(1);

      navLinks.forEach(l => l.classList.remove('active'));
      this.classList.add('active');

      sections.forEach(s => s.classList.remove('active'));
      document.getElementById(target).classList.add('active');
    });
  });

  // Tab buttons
  const tabBtns = document.querySelectorAll('.tab-btn');
  tabBtns.forEach((btn, index) => {
    btn.addEventListener('click', function() {
      tabBtns.forEach(b => b.classList.remove('active'));
      this.classList.add('active');
    });
  });

  // Drag and drop
  const dropZone = document.getElementById('drop-zone');
  if (dropZone) {
    dropZone.addEventListener('dragover', function(e) {
      e.preventDefault();
      this.classList.add('dragover');
    });

    dropZone.addEventListener('dragleave', function() {
      this.classList.remove('dragover');
    });

    dropZone.addEventListener('drop', function(e) {
      e.preventDefault();
      this.classList.remove('dragover');
    });
  }
});
