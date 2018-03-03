// TODO make these font sizes an input parameter to generating this script
GLOBAL.largeButtonSmallFontSize = 12
GLOBAL.largeButtonLargeFontSize = 18
GLOBAL.smallButtonSmallFontSize = 10
GLOBAL.smallButtonLargeFontSize = 14

// JavaScript Document

GLOBAL.buttonMouseEnterFunction = function() {
	this.start({
		'fontSize': GLOBAL.largeButtonLargeFontSize
		//'opacity' : 1.0	
	});
};

GLOBAL.buttonMouseLeaveFunction = function() {
	this.start({
		'fontSize': GLOBAL.largeButtonSmallFontSize
		//'opacity' : 0.9
	});
};

window.addEvent('domready', function() {
	
	isIE6 = navigator.userAgent.toLowerCase().indexOf('msie 6') != -1;
	
	if(isIE6) {
		GLOBAL.addLoadEvent(function() {
			var largeArrows = $$('img.largeArrow');
			largeArrows.each(function(el) {
				el.addClass('largeArrowIE6');
			});
		});
	}
	
	var links = $$('a.increaseTextSize');
	links.each(function(el) {
		el.removeClass('increaseTextSize');
	});

	var buttons = $$('span.buttonBody');
	buttons.each(function(el) {
		if(!el.hasClass('mainButtonDontAnimate')) {
			var targetElement = el;//el.getElement('span.buttonTitle');
			var tween = new Fx.Morph(targetElement, { 
				'link': 'chain',//'cancel', 
				'duration': 200, 
				'transition': 'sine:in:out'
			});
			el.addEvents({'mouseenter' : GLOBAL.buttonMouseEnterFunction.bind(tween), 
						  'mouseleave' : GLOBAL.buttonMouseLeaveFunction.bind(tween)});
		}
	});
});

