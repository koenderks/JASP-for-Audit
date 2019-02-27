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
    id: form

    Flow {
      spacing: 30

      ColumnLayout {
        RadioButtonGroup{
          name: "auditType"
          title: qsTr("<b>Materiality</b>")
          id: auditProcedure

          RowLayout {
            RadioButton { text: qsTr("Absolute")          ; name: "mus"; id: mus; checked: true}
            TextField {
              id: materialityValue
              visible: mus.checked
              name: "materialityValue"
              value: "0"
              fieldWidth: 90
              inputType: "integer"
              validator: IntValidator { bottom: 0 }
            }
          }
          RowLayout {
            RadioButton { text: qsTr("Relative")          ; name: "attributes" ; id: attributes}
            PercentField {
                id: materiality
                visible: attributes.checked
                decimals: 1
                defaultValue: 0
                name: "materiality"
            }
          }
        }

        GroupBox {
        title: qsTr("<b>Population</b>")

          TextField {
            id: populationSize
            name: "N"
            text: qsTr("Size")
            value: "0"
            fieldWidth: 90
            inputType: "integer"
            validator: IntValidator { bottom: 0 }
            enabled: mus.checked
          }
          TextField {
            id: populationValue
            name: "populationValue"
            text: qsTr("Value")
            value: "0"
            fieldWidth: 90
            inputType: "integer"
            validator: IntValidator { bottom: 0 }
            enabled: mus.checked
          }
        }
      }

        GroupBox {
            title: qsTr("<b>Audit risk</b>")
            id: auditRisk

            PercentField {
                label: qsTr("Confidence")
                decimals: 1
                defaultValue: 95
                name: "confidence"
            }
        }

        GroupBox {
            title: qsTr("<b>Explanatory text</b>")

            RowLayout {
              CheckBox {
                id: interpretationOn
                text: interpretationOn.checked ? qsTr("Enabled") : qsTr("Disabled")
                name: "interpretation"
                checked: false
                }
              MenuButton
              {
                width:				20
                iconSource:		"qrc:/images/info-button.png"
                toolTip:			"Show explanatory text in the analysis"
                radius:				20
                Layout.alignment: Qt.AlignRight
              }
            }
        }
      }

    ExpanderButton {
        text: qsTr("Advanced planning options")

        Flow {
            spacing: 20

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
                name: "expected.errors"
                id: expectedErrors
                title: qsTr("<b>Allowed errors</b>")

                RowLayout {
                    RadioButton { text: qsTr("Percentage")            ; name: "kPercentage" ; checked: true; id: expkPercentage}
                    PercentField {
                        decimals: 1
                        defaultValue: 0
                        name: "kPercentageNumber"
                        enabled: expkPercentage.checked
                    }
                }

                RowLayout {
                    RadioButton { text: qsTr("Number")              ; name: "kNumber"       ; id: expkNumber}
                    TextField {
                        value: "0"
                        name: "kNumberNumber"
                        enabled: expkNumber.checked
                        inputType: "integer"
                        validator: IntValidator { bottom: 0 }
                        Layout.leftMargin: 18
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("<b>Sampling model</b>")
                name: "distribution"
                id: distribution

                RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
                RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
            }
          }
    }

    Flow {
      spacing: 160

       GroupBox {
          title: qsTr("<b>Plots</b>")

           CheckBox {
              text: qsTr("Decision plot")
              name: "plotCriticalErrors"
              enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
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
