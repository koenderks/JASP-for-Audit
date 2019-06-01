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
      usesJaspResults: true

      GridLayout { columns: 3
          RadioButtonGroup { id: materiality; name: "materiality"; title: qsTr("Population materiality")
            RowLayout {
              RadioButton { id: materialityAbsolute; name: "materialityAbsolute"; text: qsTr("Absolute"); checked: true; childrenOnSameRow: true
                DoubleField { id: materialityValue; visible: materialityAbsolute.checked; name: "materialityValue"; defaultValue: 0; min: 0; fieldWidth: 90; decimals: 2 } }
            }
            RowLayout {
              RadioButton { id: materialityRelative; name: "materialityRelative"; text: qsTr("Relative"); childrenOnSameRow: true
                PercentField { id: materialityPercentage; visible: materialityRelative.checked; decimals: 2; defaultValue: 0; name: "materialityPercentage"; fieldWidth: 50 } }
            }
          }
          GroupBox {
              title: qsTr("Population")
              IntegerField { id: populationSize; name: "populationSize"; text: qsTr("Size"); fieldWidth: 100; defaultValue: 0  }
              DoubleField { id: populationValue; name: "populationValue"; text: qsTr("Value"); defaultValue: 0; enabled: materialityAbsolute.checked; fieldWidth: 100 }
            }
          GroupBox { title: qsTr("Audit risk"); id: auditRisk
              PercentField { name: "confidence"; label: qsTr("Confidence"); decimals: 2; defaultValue: 95 }
          }
      }
      Section { text: qsTr("Advanced options")
        GridLayout { columns: 3
            RadioButtonGroup { title: qsTr("Inherent risk"); name: "IR"; id: ir
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup { name: "expectedErrors"; id: expectedErrors; title: qsTr("Expected errors")
                RowLayout {
                    RadioButton { text: qsTr("Absolute"); name: "expectedAbsolute"; id: expectedAbsolute}
                    DoubleField { name: "expectedNumber"; enabled: expectedAbsolute.checked; defaultValue: 0; min: 0; max: 9999; decimals: 2; visible: expectedAbsolute.checked; fieldWidth: 60 }
                }
                RowLayout {
                    RadioButton { text: qsTr("Relative") ; name: "expectedRelative" ; checked: true; id: expectedRelative}
                    PercentField { name: "expectedPercentage"; enabled: expectedRelative.checked; decimals: 2; defaultValue: 0; visible: expectedRelative.checked; fieldWidth: 60  }
                }
              }
              GroupBox { title: qsTr("Explanatory text")
                RowLayout {
                  CheckBox { id: explanatoryText; text: qsTr("Enable"); name: "explanatoryText"; checked: true }
                  MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
              RadioButtonGroup { title: qsTr("Control risk"); name: "CR"; id: cr
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup {
                  title: qsTr("Planning distribution")
                  name: "planningModel"
                  id: planningModel

                  RadioButton { text: qsTr("Beta")                    ; name: "beta"; id: beta; checked: true}
                  RadioButton { text: qsTr("Beta-binomial")           ; name: "beta-binomial" ; id: betaBinomial}
              }
        }
      }
      Section { title: qsTr("Tables and plots")
        GridLayout { columns: 2
          ColumnLayout {
             GroupBox {
               title: qsTr("Statistics")

               CheckBox {      text: qsTr("Expected Bayes factor\u208B\u208A") ; name: "expectedBF"}
             }
             GroupBox {
                 title: qsTr("Tables")

                 CheckBox {      text: qsTr("Implicit sample") ; name: "implicitSampleTable"}
              }
          }

           GroupBox {
               title: qsTr("Plots")

                 CheckBox {
                    text: qsTr("Decision plot")
                    name: "decisionPlot"
                  }
               CheckBox {
                  text: qsTr("Implied prior from risk assessments")
                  name: "priorPlot"
                  id: priorPlot
                }
               PercentField {  text: qsTr("x-axis limit")                   ; name: "priorPlotLimit" ; defaultValue: 100; Layout.leftMargin: 20; enabled: priorPlot.checked}
               CheckBox {      text: qsTr("Additional info")                ; name: "priorPlotAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: priorPlot.checked}
               CheckBox { text: qsTr("Expected posterior") ; name: "priorPlotExpectedPosterior" ; Layout.leftMargin: 20; checked: false; enabled: priorPlot.checked }
            }
        }
    }

    Item {
      height: downloadReportPlanning.height
      Layout.fillWidth: true

      Button {
        id: downloadReportPlanning
        enabled: materialityRelative.checked ? (populationSize.value != 0 & materialityPercentage.value != 0) : (populationSize.value != 0 & materialityValue.value != 0 & populationValue.value != 0)
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        text: qsTr("<b>Download report</b>")
        onClicked: {
          form.exportResults()
        }
      }
    }
}
