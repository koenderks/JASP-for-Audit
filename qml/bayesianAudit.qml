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

    // Expander button for the Audit type phase
    ExpanderButton {
        text: optionPhase.expanded ? qsTr("<b>1. Audit type</b>") : qsTr("1. Audit type")
        expanded: true
        enabled: true
        id: optionPhase

        Flow {
          spacing: 160
            RadioButtonGroup{
              name: "auditType"
              title: qsTr("<b>Statement level</b>")
              id: auditProcedure

              RadioButton { text: qsTr("Percentages")           ; name: "attributes" ; checked: true; id: attributes}
              RadioButton { text: qsTr("Monetary Units")        ; name: "mus"; id: mus}
            }
            GroupBox {
                title: qsTr("<b>Explanatory text</b>")

                CheckBox { text: qsTr("Turn on")     ; name: "interpretation"; checked: true}
            }
          }

        Divider { }

        Text {
            text: qsTr("<b>Variable selection</b>")
            font.family: "SansSerif"
            font.pointSize: 12
            Layout.leftMargin: 200
        }
        // Variables form for preparation
        VariablesForm {
            availableVariablesList.name: "variablesFormPreparation"
            id: variablesFormPreparation
            implicitHeight: 100

            AssignedVariablesList {
                name: "recordNumberVariable"
                title: qsTr("Record numbers")
                singleItem: true
                allowedColumns: ["nominal", "ordinal", "scale"]
                id: recordNumberVariable
            }
            AssignedVariablesList {
                name: "monetaryVariable"
                title: qsTr("Book values")
                singleItem: true
                allowedColumns: ["scale"]
                id: monetaryVariable
            }
        }

        CheckBox {
          visible: true
          name: "planningChecked"
          id: planningChecked
          checked: false
        }
        Button {
          anchors.right: parent.right
          text: qsTr("<b>To Planning</b>")
          enabled: (recordNumberVariable.count > 0 && monetaryVariable.count > 0)
          onClicked: {
            optionPhase.expanded = false
            planningPhase.expanded = true
            planningPhase.enabled = true
            planningChecked.checked = true
          }
        }
    }

    // Expander button for the Planning phase
    ExpanderButton {
        text: planningPhase.expanded ? qsTr("<b>2. Planning</b>") : qsTr("2. Planning")
        expanded: false
        enabled: false
        id: planningPhase

      Flow {
        spacing: 20
          GroupBox {
              title: qsTr("<b>Audit risk</b>")
              id: auditRisk
              PercentField {
                  label.text: qsTr("Confidence")
                  with1Decimal: true
                  defaultValue: 95
                  name: "confidence"
              }
              PercentField {
                  label.text: qsTr("Materiality")
                  with1Decimal: true
                  defaultValue: 5
                  name: "materiality"
              }
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
              name: "expected.errors"
              id: expectedErrors
              title: qsTr("<b>Allowed errors</b>")

              RowLayout {
                  RadioButton { text: qsTr("Percentage")            ; name: "kPercentage" ; checked: true; id: expkPercentage}
                  PercentField {
                      with1Decimal: true
                      defaultValue: 2
                      name: "kPercentageNumber"
                      enabled: expkPercentage.checked
                  }
              }

              RowLayout {
                  RadioButton { text: qsTr("Number")              ; name: "kNumber"       ; id: expkNumber}
                  TextField {
                      value: "1"
                      name: "kNumberNumber"
                      enabled: expkNumber.checked
                      inputType: "integer"
                      validator: IntValidator { bottom: 0 }
                      Layout.leftMargin: 18
                  }
              }
          }
      }

    ExpanderButton {
        text: qsTr("Advanced planning options")
        Layout.leftMargin: 20
        implicitWidth: 560

        Flow {
            spacing: 60

            RadioButtonGroup {
                title: qsTr("<b>Sampling model</b>")
                name: "distribution"
                id: distribution

                RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
                RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
            }

            RadioButtonGroup {
                title: qsTr("<b>Prior distribution</b>")
                name: "prior"
                id: prior

                RadioButton { text: qsTr("Audit Risk Model")        ; name: "ARM" ; checked: true}
                RadioButton { text: qsTr("50-50")                   ; name: "5050" }
            }
          }
    }

    Divider { }

    Flow {
      spacing: 60

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

           CheckBox {      text: qsTr("Decision plot")                  ; name: "plotCriticalErrors"; checked: true }
           CheckBox {      text: qsTr("Distribution plot")              ; name: "distributionPlot"; visible: mus.checked }
           CheckBox {      text: qsTr("Implied prior and posterior")    ; name: "plotPrior" ; id: plotPrior}
           PercentField {  text: qsTr("x-axis limit")                   ; name: "limx" ; defaultValue: 20; Layout.leftMargin: 20}
           CheckBox {      text: qsTr("Additional info")                ; name: "plotPriorAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: plotPrior.checked}
        }

    }

    CheckBox {
      visible: true
      name: "samplingChecked"
      id: samplingChecked
      checked: false
    }
    Button {
      anchors.right: parent.right
      text: qsTr("<b>To Sampling</b>")
      onClicked: {
        planningPhase.expanded = false
        samplingPhase.expanded = true
        samplingPhase.enabled = true
        samplingChecked.checked = true
      }
    }
  }

  // Expander button for the Sampling phase
    ExpanderButton {
        text: samplingPhase.expanded ? qsTr("<b>3. Sampling</b>") : qsTr("3. Sampling")
        enabled: false
        expanded: false
        id: samplingPhase

        VariablesForm {
            availableVariablesList.name: "variablesFormSampling"
            id: variablesFormSampling
            implicitHeight: 200

            AssignedVariablesList {
                name: "rankingVariable"
                title: qsTr("Ranking variable (optional)")
                singleItem: true
                allowedColumns: ["scale"]
                }
            AssignedVariablesList {
                name: "variables"
                title: qsTr("Additional variables (optional)")
                singleItem: false
                allowedColumns: ["scale", "ordinal", "nominal"]
            }
        }

        Flow {
            spacing: 40

            ColumnLayout {

                RadioButtonGroup {
                    title: qsTr("<b>Seed</b>")
                    name: "seed"
                    id: seed

                    RadioButton { text: qsTr("Default")         ; name: "seedDefault" ; checked: true}
                    RowLayout {
                        RadioButton { text: qsTr("Manual")      ; name: "seedManual"  ; id: manualSeed}
                        TextField {
                            value: "1"
                            name: "seedNumber"
                            enabled: manualSeed.checked
                            validator: IntValidator { bottom: 0 }
                        }
                    }
                }

                ExpanderButton {
                  title: qsTr("Advanced sampling options")
                  implicitWidth: 260
                  id: samplingType

                  RadioButtonGroup {
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

            ColumnLayout {

                GroupBox {
                    title: qsTr("<b>Tables</b>")
                    id: samplingTables

                    CheckBox { text: qsTr("Display sample")       ; name: "showSample"}
                    CheckBox { text: qsTr("Sample descriptives")  ; name: "showDescriptives" ; id: descriptives}
                    CheckBox { text: qsTr("Mean")                 ; name: "mean"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Median")               ; name: "median"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Std. deviation")       ; name: "sd"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Variance")             ; name: "var"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Minimum")              ; name: "min"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Maximum")              ; name: "max"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Range")                ; name: "range"; Layout.leftMargin: 20; enabled: descriptives.checked}
                }
            }
        }

        Button {
          anchors.right: parent.right
          text: qsTr("<b>To Execution</b>")
          onClicked: {
            samplingPhase.expanded = false
            executionPhase.expanded = true
            executionPhase.enabled = true
          }
        }
    }

    // Expander button for the interim-evaluation option phase
    ExpanderButton {
        text: executionPhase.expanded ? qsTr("<b>4. Execution</b>") : qsTr("4. Execution")
        expanded: false
        enabled: false
        id: executionPhase

        RowLayout {
          CheckBox {
            visible: true
            name: "evaluationChecked"
            id: evaluationChecked
            checked: false
          }
        anchors.right: parent.right
          Button {
            text: qsTr("<b>Paste</b>")
            id: pasteButton
            onClicked: {
              toEvaluation.enabled = true
            }
          }
          Button {
            enabled: false
            id: toEvaluation
            text: qsTr("<b>To Evaluation</b>")
            onClicked: {
              executionPhase.expanded = false
              evaluationPhase.expanded = true
              evaluationPhase.enabled = true
              optionPhase.expanded = false
              auditProcedure.enabled = false
              auditRisk.enabled = false
              ir.enabled = false
              cr.enabled = false
              distribution.enabled = false
              expectedErrors.enabled = false
              variablesFormSampling.enabled = false
              seed.enabled = false
              samplingType.enabled = false
              prior.enabled = false
              evaluationChecked.checked = true
              pasteButton.enabled = false
              variablesFormPreparation.enabled = false
            }
          }
        }
    }

    // Expander button for the Evaluation phase
    ExpanderButton {
        text: evaluationPhase.expanded ? qsTr("<b>5. Evaluation</b>") : qsTr("5. Evaluation")
        expanded: false
        enabled: false
        id: evaluationPhase

        VariablesForm {
        availableVariablesList.name: "evaluationVariables"
        implicitHeight: 200

            AssignedVariablesList {
                name: "sampleFilter"
                title: qsTr("Sample filter")
                singleItem: true
                allowedColumns: ["nominal"]
                id: sampleFilter
            }
            AssignedVariablesList {
                visible: attributes.checked
                name: "correctID"
                title: qsTr("Error variable")
                singleItem: true
                allowedColumns: ["nominal"]
                id: correctID
            }
            AssignedVariablesList {
                visible: mus.checked
                name: "correctMUS"
                title: qsTr("True values")
                singleItem: true
                allowedColumns: ["scale"]
                id: correctMUS
            }
        }

        Flow{
          spacing: 100

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
                    text: qsTr("Confidence bound")
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
              }
          }
        }

        // Expander button for the various bounds in MUS procedure
        ExpanderButton {
          visible: attributes.checked ? false : true
          Layout.leftMargin: 20
          title: qsTr("Advanced output options")
          implicitWidth: 560

          RadioButtonGroup {
            title: qsTr("<b>Bound method</b>")
            name: "boundMethodMUS"

            RadioButton {
              name: "coxAndSnellBound"
              text: qsTr("Cox and Snell")
              checked: true
            }
          }
        }

        Button {
          anchors.right: parent.right
          enabled: attributes.checked ? (sampleFilter.count > 0 && correctID.count > 0) : (sampleFilter.count > 0 && correctMUS.count > 0)
          text: qsTr("<b>To Report</b>")
          onClicked: {
            evaluationPhase.expanded = false
            interpretationPhase.expanded = true
            interpretationPhase.enabled = true
          }
        }
    }

    // Expander button for the report phase
    ExpanderButton {
        text: interpretationPhase.expanded ? qsTr("<b>6. Report</b>") : qsTr("6. Report")
        expanded: false
        enabled: false
        id: interpretationPhase

        Button {
          anchors.right: parent.right
          text: qsTr("<b>Download Report</b>")
          onClicked: {
            interpretationPhase.expanded = false
            interpretationPhase.enabled = false
            optionPhase.enabled = false
            planningPhase.enabled = false
            samplingPhase.enabled = false
            executionPhase.enabled = false
            evaluationPhase.enabled = false
            interpretationPhase.enabled = false
          }
        }
    }
}
