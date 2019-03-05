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
    columns: 1

    // Expander button for the Planning phase
    ExpanderButton {
        text: planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning")
        id: planningPhase
        columns: 1
        expanded: true

        Flow {
          Layout.leftMargin: 20
          spacing: 150

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
                title: qsTr("<b>Audit risk</b>")
                id: auditRisk

                PercentField {
                    label: qsTr("Confidence")
                    decimals: 1
                    defaultValue: 95
                    name: "confidence"
                }
            }
          }

          Divider { }

          Text {
              text: qsTr("<b>Variable selection</b>")
              font.family: "SansSerif"
              font.pointSize: 12
              Layout.leftMargin: 200
          }

          // Variables form for planning
          VariablesForm {
              id: variablesFormPreparation
              implicitHeight: 110

              AvailableVariablesList { name: "variablesFormPreparation"}

              AssignedVariablesList {
                  name: "recordNumberVariable"
                  title: qsTr("Record numbers")
                  singleVariable: true
                  allowedColumns: ["nominal", "ordinal", "scale"]
                  id: recordNumberVariable
              }
              AssignedVariablesList {
                  name: "monetaryVariable"
                  title: qsTr("Book values")
                  singleVariable: true
                  allowedColumns: ["scale"]
                  id: monetaryVariable
              }
          }

          ExpanderButton {
              text: qsTr("Advanced planning options")

              Flow {
                  spacing: 10

                  ColumnLayout {
                    RadioButtonGroup {
                        title: qsTr("<b>Inherent risk</b>")
                        name: "IR"
                        id: ir

                        RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                        RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                        RadioButton { text: qsTr("Low")         ; name: "Low" }
                    }
                    GroupBox {
                        title: qsTr("<b>Explanatory text</b>")

                        RowLayout {
                          CheckBox {
                            id: interpretationOn
                            text: interpretationOn.checked ? qsTr("Enabled") : qsTr("Disabled")
                            name: "interpretation"
                            checked: true
                            }
                          MenuButton
                          {
                            width:				20
                            iconSource:		"qrc:/images/info-button.png"
                            toolTip:			"Show explanatory text at each step of the analysis"
                            radius:				20
                            Layout.alignment: Qt.AlignRight
                          }
                        }
                    }
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
                              fieldWidth: 40
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

                      RadioButton { text: qsTr("With replacement")            ; name: "beta"          ; checked: true}
                      RadioButton { text: qsTr("Without replacement")         ; name: "beta-binomial" ; id: hyperDist}
                  }
                }
          }

    Flow {
      spacing: 120

      ColumnLayout {
         GroupBox {
           title: qsTr("<b>Statistics</b>")

           CheckBox {      text: qsTr("Expected Bayes factor\u208B\u208A") ; name: "expectedBF"}
         }
         GroupBox {
             title: qsTr("<b>Tables</b>")

             CheckBox {      text: qsTr("Implicit sample") ; name: "implicitsample"}
          }
      }

       GroupBox {
           title: qsTr("<b>Plots</b>")

           CheckBox {
               enabled: (recordNumberVariable.count > 0 && monetaryVariable.count > 0)
               text: qsTr("Population distribution")
               name: "distributionPlot"
               id: distributionPlot
             }
             CheckBox {
                text: qsTr("Decision plot")
                name: "plotCriticalErrors"
                enabled: mus.checked ? (recordNumberVariable.count > 0 && monetaryVariable.count > 0) : true
              }
           CheckBox {
              text: qsTr("Implied prior from risk assessments")
              name: "plotPrior"
              id: plotPrior
              enabled: mus.checked ? (recordNumberVariable.count > 0 && monetaryVariable.count > 0) : true
            }
           PercentField {  text: qsTr("x-axis limit")                   ; name: "limx" ; defaultValue: 100; Layout.leftMargin: 20; enabled: plotPrior.checked}
           CheckBox {      text: qsTr("Additional info")                ; name: "plotPriorAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: plotPrior.checked}
        }
    }

    Item {
      height: toSampling.height
      Layout.fillWidth: true

      Button {
        id: downloadReportPlanning
        enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0))
        anchors.right: samplingChecked.left
        text: qsTr("<b>Download Report</b>")
      }
        CheckBox {
          anchors.right: toSampling.left
          width: height
          visible: false
          name: "samplingChecked"
          id: samplingChecked
          checked: false
        }
        Button {
          id: toSampling
          enabled: attributes.checked ? (materiality.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0)) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0))
          anchors.right: parent.right
          text: qsTr("<b>To Selection</b>")

          onClicked: {
            planningPhase.expanded = false
            samplingPhase.expanded = true
            samplingPhase.enabled = true
            samplingChecked.checked = true
          }
        }
      }
  }

  // Expander button for the Sampling phase
    ExpanderButton {
        text: samplingPhase.expanded ? qsTr("<b>2. Selection</b>") : qsTr("2. Selection")
        enabled: false
        expanded: false
        id: samplingPhase
        columns: 1

        VariablesForm {
            id: variablesFormSampling
            implicitHeight: 200

            AvailableVariablesList { name: "variablesFormSampling"}

            AssignedVariablesList {
                name: "rankingVariable"
                title: qsTr("Ranking variable (optional)")
                singleVariable: true
                allowedColumns: ["scale"]
                }
            AssignedVariablesList {
                name: "variables"
                title: qsTr("Additional variables (optional)")
                singleVariable: false
                height: 140
                allowedColumns: ["scale", "ordinal", "nominal"]
            }
        }

        Flow {
            Layout.leftMargin: 10
            spacing: 60

            ColumnLayout {

            TextField {
                value: "1"
                text: qsTr("Seed")
                name: "seedNumber"
                id: seedNumber
                validator: IntValidator { bottom: 0 }
            }

            ExpanderButton {
              title: qsTr("Advanced selection options")
              implicitWidth: 260
              id: samplingType

              ColumnLayout {

                RadioButtonGroup {
                  title: qsTr("<b>Selection type</b>")
                  name: "samplingMethod"
                  id: samplingMethod

                  RowLayout {
                    RadioButton { text: qsTr("Monetary Unit Sampling")      ; name: "mussampling" ; id: mussampling; checked: true}
                    MenuButton
                    {
                      width:				20
                      iconSource:		"qrc:/images/info-button.png"
                      toolTip:			"Select observations with probability proportional to their value"
                      radius:				20
                      Layout.alignment: Qt.AlignRight
                    }
                  }
                  RowLayout {
                    RadioButton { text: qsTr("Record Sampling")             ; name: "recordsampling" ; id: recordsampling}
                    MenuButton
                    {
                      width:				20
                      iconSource:		"qrc:/images/info-button.png"
                      toolTip:			"Select observations with equal probability"
                      radius:				20
                      Layout.alignment: Qt.AlignRight
                    }
                  }
                }

                RadioButtonGroup {
                  title: qsTr("<b>Selection method</b>")
                  name: "samplingType"

                  RadioButton { text: qsTr("Simple random sampling")                 ; name: "simplerandomsampling" ; id: simplerandomsampling; checked: true}
                  CheckBox { text: qsTr("Allow duplicate records")                   ; name: "allowDuplicates"; Layout.leftMargin: 20; enabled: simplerandomsampling.checked }
                  RadioButton { text: qsTr("Cell sampling")                   ; name: "cellsampling" ; id: cellsampling}

                  RadioButton { text: qsTr("Systematic sampling")             ; name: "systematicsampling" ; id: systematicsampling}
                  TextField {
                      text: qsTr("Interval starting point")
                      value: "1"
                      name: "startingPoint"
                      inputType: "integer"
                      validator: IntValidator { bottom: 1 }
                      Layout.leftMargin: 20
                      enabled: systematicsampling.checked
                  }
              }
          }
        }
      }

            ColumnLayout {

                GroupBox {
                    title: qsTr("<b>Tables</b>")
                    id: samplingTables

                    CheckBox { text: qsTr("Display sample")       ; name: "showSample"}
                    CheckBox { text: qsTr("Sample descriptives")  ; name: "showDescriptives" ; id: descriptives}
                    Flow {
                      Layout.leftMargin: 20
                        ColumnLayout {
                          spacing: 5
                          CheckBox { text: qsTr("Mean")                 ; name: "mean"; enabled: descriptives.checked ; checked: true}
                          CheckBox { text: qsTr("Median")               ; name: "median"; enabled: descriptives.checked ; checked: true}
                          CheckBox { text: qsTr("Std. deviation")       ; name: "sd"; enabled: descriptives.checked ; checked: true}
                          CheckBox { text: qsTr("Variance")             ; name: "var"; enabled: descriptives.checked}
                        }
                        ColumnLayout {
                          spacing: 5
                          CheckBox { text: qsTr("Minimum")              ; name: "min"; enabled: descriptives.checked}
                          CheckBox { text: qsTr("Maximum")              ; name: "max"; enabled: descriptives.checked}
                          CheckBox { text: qsTr("Range")                ; name: "range"; enabled: descriptives.checked}
                        }
                    }
                }
            }
        }

        Item {
          height: toExecution.height
          Layout.fillWidth: true

          Button {
            id: downloadReportSelection
            enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
            anchors.right: executionChecked.left
            text: qsTr("<b>Download Report</b>")
          }

          CheckBox {
            anchors.right: toExecution.left
            width: height
            visible: false
            name: "executionChecked"
            id: executionChecked
            checked: false
          }

            Button {
              id: toExecution
              anchors.right: parent.right
              text: qsTr("<b>To Execution</b>")
              onClicked: {
                samplingPhase.expanded = false
                executionPhase.expanded = true
                executionPhase.enabled = true
              }
            }
        }
    }

    // Expander button for the interim-evaluation option phase
    ExpanderButton {
        text: executionPhase.expanded ? qsTr("<b>3. Execution</b>") : qsTr("3. Execution")
        expanded: false
        enabled: false
        id: executionPhase
        columns: 1

        Text {
            text: qsTr("<b>How would you like to evaluate your observations?</b>")
            font.family: "SansSerif"
            font.pointSize: 10
            Layout.leftMargin: 80
        }

        RadioButtonGroup {
          Layout.leftMargin: 50
          name: "variableType"
          id: variableType
          title: qsTr("")

          RowLayout {
            spacing: 150

            RowLayout {
              RadioButton { text: qsTr("Audit values")                 ; name: "variableTypeTrueValues" ; id: variableTypeTrueValues; checked: true }
              MenuButton
              {
                width:				20
                iconSource:		"qrc:/images/info-button.png"
                toolTip:			"Adds a column to specify the audit value of the observations"
                radius:				20
                Layout.alignment: Qt.AlignRight
              }
            }
            RowLayout {
              RadioButton { text: qsTr("Correct / Incorrect")                     ; name: "variableTypeCorrect" ; id: variableTypeCorrect }
              MenuButton
              {
                width:				20
                iconSource:		"qrc:/images/info-button.png"
                toolTip:			"Adds a column to specify the observations as correct (0) or incorrect (1)"
                radius:				20
                Layout.alignment: Qt.AlignRight
              }
            }
          }
        }

        Item {
          height: toEvaluation.height
          Layout.fillWidth: true

          CheckBox {
            anchors.right: pasteButton.left
            width: height
            visible: false
            name: "pasteVariables"
            id: pasteVariables
            checked: false
          }

          Button {
            text: qsTr("<b>Add Variables</b>")
            id: pasteButton
            anchors.right: evaluationChecked.left
            onClicked: {
              toEvaluation.enabled = true
              pasteButton.enabled = false
              pasteVariables.checked = true
              variableType.enabled = false
              auditProcedure.enabled = false
              auditRisk.enabled = false
              ir.enabled = false
              cr.enabled = false
              distribution.enabled = false
              expectedErrors.enabled = false
              variablesFormSampling.enabled = false
              seedNumber.enabled = false
              samplingType.enabled = false
              evaluationChecked.checked = true
              pasteButton.enabled = false
              variablesFormPreparation.enabled = false
              samplingMethod.enabled = false
            }
          }

          CheckBox {
            anchors.right: toEvaluation.left
            width: height
            visible: false
            name: "evaluationChecked"
            id: evaluationChecked
            checked: false
          }

          Button {
            enabled: false
            id: toEvaluation
            anchors.right: parent.right
            text: qsTr("<b>To Evaluation</b>")
            onClicked: {
              executionPhase.expanded = false
              evaluationPhase.expanded = true
              evaluationPhase.enabled = true
            }
          }
        }
    }

    // Expander button for the Evaluation phase
    ExpanderButton {
        text: evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation")
        expanded: false
        enabled: false
        id: evaluationPhase
        columns: 1

        VariablesForm {
        implicitHeight: 200

        AvailableVariablesList { name: "evaluationVariables"}

            AssignedVariablesList {
                name: "sampleFilter"
                title: qsTr("Sample filter")
                singleVariable: true
                allowedColumns: ["nominal"]
                id: sampleFilter
            }
            AssignedVariablesList {
                visible: variableTypeCorrect.checked
                name: "correctID"
                title: qsTr("Error variable")
                singleVariable: true
                allowedColumns: ["nominal"]
                id: correctID
            }
            AssignedVariablesList {
                visible: variableTypeTrueValues.checked
                name: "correctMUS"
                title: qsTr("Audit values")
                singleVariable: true
                allowedColumns: ["scale"]
                id: correctMUS
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
                    visible: variableTypeTrueValues.checked
                  }
              }
          }
        }

        // Expander button for the various bounds
        ExpanderButton {
          title: qsTr("Advanced evaluation options")
          columns: 1

          RadioButtonGroup {
            title: qsTr("<b>Estimator</b>")
            name: "boundMethod"

            RadioButton {
              name: "coxAndSnellBound"
              text: qsTr("Cox and Snell")
              id: coxAndSnellBound
              visible: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false
              enabled: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false
              checked: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false
            }
            RadioButton {
              name: "binomialBound"
              text: qsTr("Binomial")
              id: binomialBound
              visible: variableTypeCorrect.checked
              enabled: variableTypeCorrect.checked
              checked: variableTypeCorrect.checked
            }
            RadioButton {
              name: "regressionBound"
              text: qsTr("Regression")
              id: regressionBound
              visible: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false
              enabled: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false
              checked: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false
            }
          }
        }

        Item {
          height: toInterpretation.height
          Layout.fillWidth: true

          Button {
            id: toInterpretation
            anchors.right: parent.right
            enabled: variableTypeCorrect.checked ? (sampleFilter.count > 0 && correctID.count > 0) : (sampleFilter.count > 0 && correctMUS.count > 0)
            text: qsTr("<b>Download Report</b>")
            onClicked: {
              evaluationPhase.expanded = false
              interpretationPhase.expanded = true
              interpretationPhase.enabled = true
            }
          }
        }
    }
}
