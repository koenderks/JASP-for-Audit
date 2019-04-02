//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {

    VariablesForm { implicitHeight: 200
      AvailableVariablesList { name: "evaluationVariables"}
      AssignedVariablesList { name: "sampleFilter"; title: qsTr("Selection result"); singleVariable: true; allowedColumns: ["nominal"]; id: sampleFilter }
      AssignedVariablesList { name: "correctID"; title: qsTr("Audit result"); singleVariable: true; allowedColumns: ["nominal" ,"scale"]; id: correctID }
    }

    GridLayout { columns: 3
        RadioButtonGroup { id: auditType; name: "auditType"; title: qsTr("<b>Materiality</b>")
          RowLayout {
            RadioButton { id: mus; name: "mus"; text: qsTr("Absolute"); checked: true; childrenOnSameRow: true
              DoubleField { id: materialityValue; visible: mus.checked; name: "materialityValue"; defaultValue: 0; min: 0; fieldWidth: 90; decimals: 2 } }
          }
          RowLayout {
            RadioButton { id: attributes; name: "attributes"; text: qsTr("Relative"); childrenOnSameRow: true
              PercentField { id: materiality; visible: attributes.checked; decimals: 2; defaultValue: 0; name: "materiality"; fieldWidth: 50 } }
          }
        }
        GroupBox {
            title: qsTr("<b>Population</b>")
            IntegerField { id: populationSize; name: "populationSize"; text: qsTr("Size"); fieldWidth: 100; defaultValue: 0  }
            DoubleField { id: populationValue; name: "populationValue"; text: qsTr("Value"); defaultValue: 0; enabled: mus.checked; fieldWidth: 100 }
          }
        GroupBox { title: qsTr("<b>Audit risk</b>"); id: auditRisk
            PercentField { name: "confidence"; label: qsTr("Confidence"); decimals: 2; defaultValue: 95 }
        }
    }

    Section {
      title: qsTr("Advanced options")

      GridLayout {
        columns: 4

        RadioButtonGroup {
            title: qsTr("<b>Inherent risk</b>")
            name: "IR"
            id: ir

            RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
            RadioButton { text: qsTr("Medium")      ; name: "Medium" }
            RadioButton { text: qsTr("Low")         ; name: "Low" }
        }
        RadioButtonGroup {
            title: qsTr("<b>Control risk</b>")
            name: "CR"
            id: cr

            RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
            RadioButton { text: qsTr("Medium")      ; name: "Medium" }
            RadioButton { text: qsTr("Low")         ; name: "Low" }
        }

        RadioButtonGroup {
          title: qsTr("<b>Estimator</b>")
          name: "boundMethod"

          RadioButton { name: "stringerBound"; text: qsTr("Stringer"); id: stringerBound }
          RadioButton { name: "directBound"; text: qsTr("Direct"); id: directBound }
          RadioButton { name: "differenceBound"; text: qsTr("Difference"); id: differenceBound }
          RadioButton { name: "ratioBound"; text: qsTr("Ratio"); id: ratioBound; visible: false }
          RadioButton { name: "regressionBound"; text: qsTr("Regression"); id: regressionBound }
          RadioButton { name: "gammaBound"; text: qsTr("Gamma"); id: gammaBound; visible: false }
          RadioButton { name: "binomialBound"; text: qsTr("Binomial"); id: binomialBound }
          RadioButton { name: "hyperBound"; text: qsTr("Hypergeometric"); id: hyperBound }
        }
        GroupBox { title: qsTr("<b>Explanatory text</b>")
          RowLayout {
            CheckBox { id: interpretationOn; text: qsTr("Enable"); name: "interpretation"; checked: true }
            MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
          }
        }
      }
    }
    
    Section {
      title: qsTr("Tables and Plots")
      
      GridLayout {
      
        GroupBox {
          title: qsTr("<b>Statistics</b>")

          CheckBox {
              text: qsTr("Most Likely Error (MLE)")
              name: "mostLikelyError"
              checked: false
          }
        }
        
        GroupBox {
            title: qsTr("<b>Plots</b>")
            CheckBox {
              text: qsTr("Evaluation information")
              name: "plotBound"
            }
            CheckBox {
              text: qsTr("Correlation plot")
              name: "plotCorrelation"
              enabled: variableTypeTrueValues.checked
            }
        }        
      }
    }
    
    Item {
      height: downloadReportEvaluation.height
      Layout.fillWidth: true
      anchors.bottom: parent.bottom

      Button {
        id: downloadReportEvaluation
        enabled: attributes.checked ? (populationSize.value != 0 & materiality.value != 0 & correctID.count > 0 & sampleFilter.count > 0) : (populationSize.value != 0 & materialityValue.value != 0 & populationValue.value != 0 & correctID.count > 0 & sampleFilter.count > 0)
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        text: qsTr("<b>Download Report</b>")
      }
    }
}
