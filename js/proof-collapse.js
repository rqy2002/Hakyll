$(document).ready(function () {
  setInterval(() => {
    // Proof expander
    if ($('.proof-collapsible:not(.initialized)').length > 0) {
      $('.proof-collapsible').addClass('initialized');
      $('.proof-expander.proof-expander-expanding').html('<svg height="16" width="16"><path stroke="#999" stroke-width="1.5" fill="none" d="M8 2 L14 8 L8 14"></path></svg> ');
      $('.proof-expander.proof-expander-collapsing').html('<svg height="16" width="16"><path stroke="#999" stroke-width="1.5" fill="none" d="M2 6 L8 12 L14 6"></path></svg> ');
      $('.proof-expander.proof-expander-ellipsis').html('<span style="font-size:40%">• • •</span>');
      $('.proof-expander').click(function () {
        let $this = $(this);
        let $parent = $this.closest('.proof-collapsible');
        if ($parent.length === 0) return;
    
        let wasCollapsed = !$this.hasClass('proof-expander-collapsing');
        $parent.removeClass('proof-collapsible-collapsed');
        $parent.removeClass('proof-collapsible-expanded');
        $parent.addClass(wasCollapsed ? 'proof-collapsible-expanded' : 'proof-collapsible-collapsed');
      });
    }
  }, 500);
});
