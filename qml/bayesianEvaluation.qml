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

    VariablesForm {
    implicitHeight: 200

    AvailableVariablesList { name: "evaluationVariables"}

        AssignedVariablesList {
            enabled: true
            name: "monetaryVariable"
            title: qsTr("Book values")
            singleVariable: true
            allowedColumns: ["scale"]
            id: monetaryVariable
        }
        AssignedVariablesList {
            enabled: variableTypeTrueValues.checked
            name: "correctMUS"
            title: qsTr("Audit values")
            singleVariable: true
            allowedColumns: ["scale"]
            id: correctMUS
        }
        AssignedVariablesList {
            enabled: variableTypeCorrect.checked
            name: "correctID"
            title: qsTr("Error variable")
            singleVariable: true
            allowedColumns: ["nominal"]
            id: correctID
        }
    }

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

    // Expander button for the various bounds
    ExpanderButton {
      visible: true
      title: qsTr("Advanced evaluation options")
      columns: 1

      Flow{
        spacing: 30

        RadioButtonGroup {
          title: qsTr("<b>Assessment</b>")
          name: "variableType"

              RadioButton { text: qsTr("Audit values")                 ; name: "variableTypeTrueValues" ; id: variableTypeTrueValues; checked: true }
              RadioButton { text: qsTr("Correct / Incorrect")          ; name: "variableTypeCorrect"    ; id: variableTypeCorrect }
        }

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

        RadioButton {
          name: "coxAndSnellBound"
          text: qsTr("Cox and Snell")
          id: coxAndSnellBound
          visible: variableTypeTrueValues.checked
          enabled: variableTypeTrueValues.checked
          checked: variableTypeTrueValues.checked ? true : false
        }
        RadioButton {
          name: "betaBound"
          text: qsTr("Beta")
          id: betaBound
          visible: variableTypeCorrect.checked
          enabled: variableTypeCorrect.checked
          checked: variableTypeCorrect.checked ? true : false
        }
        RadioButton {
          name: "regressionBound"
          text: qsTr("Regression")
          id: regressionBound
          visible: variableTypeTrueValues.checked
          enabled: variableTypeTrueValues.checked
          checked: false
        }
      }
    }
    }

    Flow{
      Layout.leftMargin: 10
      spacing: 140

      GroupBox {
        title: qsTr("<b>Statistics</b>")

        CheckBox {
            text: qsTr("Most Likely Error (MLE)")
            name: "mostLikelyError"
            checked: false
        }
        CheckBox {
            text: qsTr("Bayes factor\u208B\u208A")
            name: "bayesFactor"
        }

      }

      ColumnLayout {
          GroupBox {
              title: qsTr("<b>Plots</b>")
              CheckBox {
                text: qsTr("Evaluation information")
                name: "plotBound"
              }
                CheckBox {
                    text: qsTr("Prior and posterior")
                    name: "plotPriorAndPosterior"
                    id: plotPriorAndPosterior
                }
                PercentField {
                  text: qsTr("x-axis limit")
                  defaultValue: 20
                  name: "limx_backup"
                  Layout.leftMargin: 20
                }
              CheckBox {
                text: qsTr("Additional info")
                name: "plotPriorAndPosteriorAdditionalInfo"
                Layout.leftMargin: 20
                checked: true
                enabled: plotPriorAndPosterior.checked
              }
              CheckBox {
                text: qsTr("Correlation plot")
                name: "plotCorrelation"
                enabled: variableTypeTrueValues.checked
              }
          }
      }
    }
}
