// JavaScript Document

GLOBAL.getFadeInObject = function( idOfElementToFadeIn ) {
	
	fadeObject = {};
	
	fadeObject.setup = function( fadeInDuration ) {
		
		var element = $(idOfElementToFadeIn);
		
		this.fadeInTween = new Fx.Tween( element, { 
			'transition' : 'quad:in',
			'duration' : fadeInDuration
		});
		
		this.fadeInTween.set( 'opacity', 0.0);
		return fadeInDuration;
	};
	
	fadeObject.start = function() {
		this.fadeInTween.start('opacity', 1.0 );
	};
	
	return fadeObject;
};


GLOBAL.getFadeInObjectForClass = function( className ) {
	
	fadeObject = {};
	
	fadeObject.setup = function( fadeInDuration ) {
		
		this.elements = $$(className);
		
		this.elements.each( function(el) {
			el.fadeInTween = new Fx.Tween( el, { 
				'transition' : 'quad:in',
				'duration' : fadeInDuration
			});
			
			el.fadeInTween.set( 'opacity', 0.0 );
		});
		
		return fadeInDuration;
	};
	
	fadeObject.start = function() {
		
		this.elements.each( function(el) {
			el.fadeInTween.start('opacity', 1.0);
		});
	};
	
	return fadeObject;
};




GLOBAL.getButtonDropDownObject = function(classDescriptor) {
	return {
		setup : function ( buttonFadeInDuration, pauseAfterFadeIn, buttonFallDuration, pauseBetweenFalls ) {
			this.buttonFadeInDuration = buttonFadeInDuration;
			this.pauseAfterFadeIn = pauseAfterFadeIn;
			this.buttonFallDuration = buttonFallDuration;
			this.pauseBetweenFalls = pauseBetweenFalls;
			
			this.buttons = $$(classDescriptor);
			
			var numberOfButtons = this.buttons.length;
			var buttonPadding = parseInt(this.buttons[0].getStyle('padding-bottom').substitute('px', ''));
			var buttonHeight = parseInt(this.buttons[0].getStyle('height').substitute('px', ''));
			var totalButtonHeight = buttonHeight + ( buttonPadding );
			
			var increment = 0;
			this.buttons.each(function (el) { 
				if( increment > 0 ) {
					
					el.dropDownMorph = new Fx.Morph(el, {
						'duration' : buttonFallDuration,
						'transition' : 'back:out'//'elastic:out'//'bounce:out'
					});
					
					el.dropDownMorph.set({
						'position' : 'relative',
						'z-index' : (numberOfButtons - increment + 1),
						'top' : -totalButtonHeight,
						'opacity' : 0.0
					});
				}
				else {
					
					el.fadeInMorph = new Fx.Morph(el, {
						'duration' : buttonFadeInDuration,
						'transition' : 'sine:in:out'
					});
					
					el.fadeInMorph.set({
						'position' : 'relative',
						'z-index' : (numberOfButtons - increment + 1),
						'opacity' : 0.0
					});
				}
				increment++;
			});
			
			var buttonFallTime = (this.buttons.length -1) * (this.buttonFallDuration + this.pauseBetweenFalls);
			var totalTime = this.buttonFadeInDuration + this.pauseAfterFadeIn + buttonFallTime;
			return totalTime;
		},
		
		start: function () {
			var increment = 0;
			var buttonFallDuration = this.buttonFallDuration;
			var pauseAfterFadeIn = this.pauseAfterFadeIn;
			var buttonFadeInDuration = this.buttonFadeInDuration;
			var pauseBetweenFalls = this.pauseBetweenFalls;
			
			this.buttons.each(function(el) { 
				if( increment > 0 ) {
					var calculatedPause = (increment-1) * (buttonFallDuration + pauseBetweenFalls) + (pauseAfterFadeIn + buttonFadeInDuration);
					el.dropDownMorph.set.delay( calculatedPause, el.dropDownMorph, {
						'opacity' : 1.0
					});
				
					el.dropDownMorph.start.delay( calculatedPause, el.dropDownMorph, {
						'top' : 0
					});
				}
				else {
					el.fadeInMorph.start({
						'opacity' : 1.0
					});
				}
				increment++;
			});
		}
	};
};

GLOBAL.addLoadEvent = function(func) {
  var oldonload = window.onload;
  if (typeof window.onload != 'function') {
    window.onload = func;
  } 
  else {
    window.onload = function() {
      if (oldonload) {
        oldonload();
      }
      func();
    };
  }
};

