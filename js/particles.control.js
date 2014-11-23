var inter = true;
if (skel.isActive('1000px')) {
  inter = false;
}
if (skel.isActive('desktop')) {
  particlesJS('particles-js', {
      particles: {
          color: '#777',
          shape: 'circle',
          opacity: 0.5,
          size: 2.5,
          size_random: true,
          nb: 100,
          line_linked: {
              enable_auto: true,
              distance: 300,
              color: '#777',
              opacity: 0.4,
              width: 1,
              condensed_mode: {
                  enable: false,
                  rotateX: 600,
                  rotateY: 600
              }
          },
          anim: {
              enable: true,
              speed: 0.25
          }
      },
      interactivity: {
          enable: inter,
          mouse: {
              distance: 200
          },
          mode: 'grab'
      },
      retina_detect: false
  });
}
