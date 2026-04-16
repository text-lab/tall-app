// TALL — editorial site JS
// Handles: scroll-triggered navbar state, fade-in on intersection,
// copy-to-clipboard, active nav link highlighting, mobile menu toggle.

(() => {
  'use strict';

  // ---- Mobile menu toggle ----
  const burger = document.querySelector('.nav-burger');
  const mobileMenu = document.querySelector('.mobile-menu');
  if (burger && mobileMenu) {
    burger.addEventListener('click', () => {
      const open = mobileMenu.style.display === 'block';
      mobileMenu.style.display = open ? 'none' : 'block';
      burger.setAttribute('aria-expanded', String(!open));
    });
  }

  // ---- Active nav link ----
  const currentPage = (window.location.pathname.split('/').pop() || 'index.html').toLowerCase();
  document.querySelectorAll('.nav-links a, .mobile-menu a').forEach(link => {
    const href = (link.getAttribute('href') || '').toLowerCase();
    if (href === currentPage || (currentPage === '' && href === 'index.html')) {
      link.classList.add('active');
    }
  });

  // ---- Navbar scroll state ----
  const nav = document.querySelector('.site-nav');
  if (nav) {
    const onScroll = () => {
      if (window.scrollY > 12) nav.classList.add('scrolled');
      else nav.classList.remove('scrolled');
    };
    onScroll();
    window.addEventListener('scroll', onScroll, { passive: true });
  }

  // ---- Intersection-triggered fade-in ----
  const io = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add('visible');
        io.unobserve(entry.target);
      }
    });
  }, { threshold: 0.1, rootMargin: '0px 0px -60px 0px' });

  document.querySelectorAll('.fade-in').forEach(el => io.observe(el));
})();

// ---- Copy-to-clipboard ----
function copyCode(btn) {
  const block = btn.closest('.code-block');
  if (!block) return;
  const code = block.querySelector('code');
  if (!code) return;
  navigator.clipboard.writeText(code.textContent.trim()).then(() => {
    const prev = btn.textContent;
    btn.textContent = 'Copied';
    btn.style.color = '#4DB280';
    setTimeout(() => {
      btn.textContent = prev;
      btn.style.color = '';
    }, 1800);
  });
}
