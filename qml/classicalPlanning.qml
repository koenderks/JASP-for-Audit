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
      Section { text: qsTr("Advanced planning options")
        GridLayout { columns: 3
            RadioButtonGroup { title: qsTr("<b>Inherent risk</b>"); name: "IR"; id: ir
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup { name: "expected.errors"; id: expectedErrors; title: qsTr("<b>Expected errors</b>")
                RowLayout {
                    RadioButton { text: qsTr("Absolute"); name: "kNumber"; id: expkNumber}
                    DoubleField { name: "kNumberNumber"; enabled: expkNumber.checked; defaultValue: 0; min: 0; max: 9999; decimals: 2; visible: expkNumber.checked; fieldWidth: 60 }
                }
                RowLayout {
                    RadioButton { text: qsTr("Relative") ; name: "kPercentage" ; checked: true; id: expkPercentage}
                    PercentField { name: "kPercentageNumber"; enabled: expkPercentage.checked; decimals: 2; defaultValue: 0; visible: expkPercentage.checked; fieldWidth: 60  }
                }
              }
              GroupBox { title: qsTr("<b>Explanatory text</b>")
                RowLayout {
                  CheckBox { id: interpretationOn; text: qsTr("Enable"); name: "interpretation"; checked: true }
                  MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
              RadioButtonGroup { title: qsTr("<b>Control risk</b>"); name: "CR"; id: cr
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup {
                  title: qsTr("<b>Sampling model</b>")
                  name: "distribution"
                  id: distribution
                  
                  RadioButton { text: qsTr("Monetary units")                       ; name: "gamma" ; checked: true; id: gamma}
                  RadioButton { text: qsTr("With replacement")                    ; name: "binomial"; id: binomial}
                  RadioButton { text: qsTr("Without replacement")                 ; name: "hypergeometric" ; id: hyperDist}
              }
        }
      }
      Section { title: qsTr("Tables and plots")
      GridLayout { columns: 2  
        GroupBox { title: qsTr("<b>Plots</b>")
          CheckBox { text: qsTr("Decision plot"); name: "plotCriticalErrors" }
        }
      }
      }

      Item {
        height: downloadReportPlanning.height
        Layout.fillWidth: true

        Button {
          id: downloadReportPlanning
          enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0))
          anchors.right: parent.right
          anchors.bottom: parent.bottom
          text: qsTr("<b>Download Report</b>")
        }
      }
}
