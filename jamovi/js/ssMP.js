'use strict';

module.exports = {

    view_loaded: function(ui, event) {
        this.updateBoxes(ui);
    },

    view_updated: function(ui, event) {
        this.updateBoxes(ui);
    },

    tipo_changed: function(ui, event) {
        this.updateBoxes(ui);
    },

    updateBoxes: function(ui) {

        const analysisType = ui.tipo.value();
        
        //estas instrucciones "ocultan" los collapseboxes de los parámetros inicialmente
        ui.oneMeanBox.$el.hide();
        ui.oneProportionBox.$el.hide();
        ui.independentMeansBox.$el.hide();
        ui.pairedMeansBox.$el.hide();
        ui.independentProportionsBox.$el.hide();

        //estos ifs muestran el collapsebox de los parámetros según el análisis
        //elegido en el desplegable
        if (analysisType === 'media') {
            ui.oneMeanBox.$el.show();
            
        }
        else if (analysisType === 'proporcion') {
            ui.oneProportionBox.$el.show();
        }
        else if (analysisType === 'dosMediasVD') {
            ui.independentMeansBox.$el.show();
        }
        else if (analysisType === 'dosMediasVC') {
            ui.pairedMeansBox.$el.show();
        }
        else if (analysisType === 'dosProporciones') {
            ui.independentProportionsBox.$el.show();
        }
    }
};