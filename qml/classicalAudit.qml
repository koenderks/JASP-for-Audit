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

    // Expander button for the Global options
    ExpanderButton {
        text: optionPhase.expanded ? qsTr("<b>1. Global options</b>") : qsTr("1. Global options")
        expanded: toPlanning.checked ? false : true
        enabled: toPlanning.checked ? false : true
        id: optionPhase

        Flow {
          spacing: 40

          RadioButtonGroup{
            name: "auditType"
            title: qsTr("<b>Audit type</b>")

            RadioButton { text: qsTr("Attributes sampling")           ; name: "attributes" ; checked: true; id: attributes}
            RadioButton { text: qsTr("Monetary unit sampling")        ; name: "mus"; id: mus}
          }

          RadioButtonGroup {
              title: qsTr("<b>Units</b>")
              name: "show"

              RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true; id: percentages}
              RadioButton { text: qsTr("Proportions")         ; name: "proportion"; id: proportions}
          }

          GroupBox {
              title: qsTr("<b>Interpretation</b>")

              CheckBox { text: qsTr("Toggle interpretation")     ; name: "interpretation"; checked: true}
          }
        }

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>To planning</b>")
        }

        CheckBox {
          name: "toPlanning"
          Layout.leftMargin: 450
          text: qsTr("<b>To planning</b>")
          id: toPlanning
        }

    }

    // Expander button for the Planning phase
    ExpanderButton {
        text: planningPhase.enabled ? qsTr("<b>2. Planning</b>") : qsTr("2. Planning")
        expanded: toSampling.checked ? false : (toPlanning.checked ? true : false)
        enabled: toSampling.checked ? false : (toPlanning.checked ? true : false)
        id: planningPhase

    Flow {
        spacing: 40

        ColumnLayout {
          GroupBox {
              title: qsTr("<b>Audit risk</b>")

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

            TextField {
                text: qsTr("Population size")
                value: "0"
                name: "N"
                inputType: "integer"
                validator: IntValidator { bottom: 0 }
                id: populationSize
            }
          }
        }

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Inherent risk</b>")
                name: "IR"
                enabled: populationSize.value == "0" ? false : true

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Control risk</b>")
                name: "CR"
                enabled: populationSize.value == "0" ? false : true

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }
    }

    Divider { }

    Flow {
        spacing: 40

        RadioButtonGroup {
            title: qsTr("<b>Sampling model</b>")
            name: "distribution"
            enabled: populationSize.value == "0" ? false : true

            RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
            RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
        }

          RadioButtonGroup {
              title: qsTr("<b>Allowed errors</b>")
              name: "expected.errors"
              enabled: populationSize.value == "0" ? false : true

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

    Divider { }

    Flow {
      spacing: 90

       GroupBox {
           title: qsTr("<b>Plots</b>")
           enabled: populationSize.value == "0" ? false : true

           CheckBox {      text: qsTr("Decision plot")   ; name: "plotCriticalErrors"; checked: true }

        }

    }

    Button {
      Layout.leftMargin: 450
      text: qsTr("<b>To sampling</b>")
      enabled: populationSize.value == "0" ? false : true
    }

    CheckBox {
      name: "toSampling"
      Layout.leftMargin: 450
      text: qsTr("<b>To sampling</b>")
      id: toSampling
      enabled: populationSize.value == "0" ? false : true
    }
  }

  // Expander button for the sampling phase
  ExpanderButton {
      text: samplingPhase.expanded ? qsTr("<b>3. Sampling</b>") : qsTr("3. Sampling")
      expanded: toSampling.checked ? (toEvaluation.checked ? false : true) : false
      enabled: toSampling.checked ? (toEvaluation.checked ? false : true) : false
      id: samplingPhase

      ExpanderButton {
          expanded: pickSamplingMethod.checked ? false : true
          //enabled: pickSamplingMethod.checked ? false : true
          title: samplingMethod.expanded ? qsTr("<b>3.1 Sampling method</b>") : qsTr("3.1 Sampling method")
          implicitWidth: 570
          id: samplingMethod
          Layout.leftMargin: 20

          RowLayout {
            Label {
              text: qsTr("<b>Random</b>")
              }
            Divider {
              implicitWidth: 420
            }
            Label {
              text: qsTr("<b>Systematic</b>")
              }
          }

          RadioButtonGroup {
            name: "samplingType"
            Layout.leftMargin: 20

            Flow {
              spacing: 20

                ColumnLayout {
                  spacing: 5
                  RadioButton { text: qsTr("Simple random sampling")                 ; name: "simplerandomsampling" ; id: simplerandomsampling; checked: true}
                  CheckBox { text: qsTr("Allow duplicate records")                   ; name: "allowDuplicates"; Layout.leftMargin: 20; enabled: simplerandomsampling.checked }
                }
                ColumnLayout {
                  RadioButton { text: qsTr("Cell sampling")                   ; name: "cellsampling" ; id: cellsampling}
                }
                ColumnLayout {
                  spacing: 5
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

          Button {
            Layout.leftMargin: 450
            text: qsTr("<b>Confirm</b>")
          }

          CheckBox {
            name: "pickSamplingMethod"
            Layout.leftMargin: 450
            text: qsTr("<b>Confirm</b>")
            id: pickSamplingMethod
          }

      }

      // Variables form for attributes sampling
      VariablesForm {
          availableVariablesList.name: "allAvailableVariables2"
          enabled: pickSamplingMethod.checked ? true : false
          visible: attributes.checked ? true : false

          AssignedVariablesList {
              name: "recordNumberVariable"
              title: qsTr("Record numbers")
              singleItem: true
              allowedColumns: ["nominal", "ordinal", "scale"]
              enabled: pickSamplingMethod.checked ? true : false
          }
          AssignedVariablesList {
              name: "rankingVariable"
              title: qsTr("Ranking variable (optional)")
              singleItem: true
              allowedColumns: ["scale"]
              }
          AssignedVariablesList {
              name: "variables"
              title: qsTr("Sampling variables")
              singleItem: false
              allowedColumns: ["scale", "ordinal", "nominal"]
          }
      }

      // Variables form for MUS sampling
      VariablesForm {
          availableVariablesList.name: "allAvailableVariables3"
          enabled: pickSamplingMethodMUS.checked ? true : false
          visible: attributes.checked ? false : true

          AssignedVariablesList {
              name: "recordNumberVariableMUS"
              title: qsTr("Record numbers")
              singleItem: true
              allowedColumns: ["nominal", "ordinal", "scale"]
              enabled: pickSamplingMethodMUS.checked ? true : false
          }
          AssignedVariablesList {
              name: "monetaryVariableMUS"
              title: qsTr("Monetary values")
              singleItem: true
              allowedColumns: ["scale"]
              }
          AssignedVariablesList {
              name: "rankingVariableMUS"
              title: qsTr("Ranking variable (optional)")
              singleItem: true
              allowedColumns: ["scale"]
              }
          AssignedVariablesList {
              name: "variablesMUS"
              title: qsTr("Sampling variables")
              singleItem: false
              allowedColumns: ["scale", "ordinal", "nominal"]
          }
      }

      Flow {
          spacing: 220

          ColumnLayout {

              RadioButtonGroup {
                  title: qsTr("<b>Seed</b>")
                  name: "seed"
                  enabled: pickSamplingMethod.checked ? true : false

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
          }

          ColumnLayout {

              GroupBox {
                  title: qsTr("<b>Tables</b>")
                  enabled: pickSamplingMethod.checked ? true : false

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
        Layout.leftMargin: 450
        text: qsTr("<b>To evaluation</b>")
      }

      CheckBox {
        name: "toEvaluation"
        Layout.leftMargin: 450
        text: qsTr("<b>To evaluation</b>")
        id: toEvaluation
      }

  }

  // Expander button for the evaluation phase
    ExpanderButton {
        text: evaluationPhase.enabled ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation")
        expanded: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? false : true) : false) : false
        enabled: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? false : true) : false) : false
        id: evaluationPhase

        // Variables form for attributes evaluation
        VariablesForm {
        visible: attributes.checked ? true : false
        availableVariablesList.name: "allAvailableVariables4"

            AssignedVariablesList {
                name: "correctID"
                title: qsTr("Error variable")
                singleItem: true
                allowedColumns: ["nominal"]
            }
            AssignedVariablesList {
                name: "sampleFilter"
                title: qsTr("Sample filter variable")
                singleItem: true
                allowedColumns: ["nominal"]
            }
        }

        // Variables form for attributes evaluation
        VariablesForm {
        visible: attributes.checked ? false : true
        availableVariablesList.name: "allAvailableVariables5"

            AssignedVariablesList {
                name: "correctMUS"
                title: qsTr("True monetary values")
                singleItem: true
                allowedColumns: ["scale"]
            }
            AssignedVariablesList {
                name: "sampleFilterMUS"
                title: qsTr("Sample filter variable")
                singleItem: true
                allowedColumns: ["nominal"]
            }
        }

        Flow{
          spacing: 100

          GroupBox {
            title: qsTr("<b>Tables</b>")

            CheckBox {
                text: qsTr("Most likely error")
                name: "mostLikelyError"
                checked: false
            }

          }

          ColumnLayout {
              GroupBox {
                  title: qsTr("<b>Plots</b>")
                    CheckBox {
                        text: qsTr("Confidence bound")
                        name: "plotBound"
                        enabled: frequentistType.checked
                    }
                }
            }
        }

        // Expander button for the various bounds in MUS procedure
        ExpanderButton {
          visible: attributes.checked ? false : true
          Layout.leftMargin: 20
          title: qsTr("Advanced output options")
          implicitWidth: 570

          RadioButtonGroup {
            title: qsTr("<b>Confidence bound</b>")
            name: "boundMethodMUS"

            RadioButton {
              name: "stringerBound"
              text: qsTr("Stringer bound")
              checked: true
            }
          }
        }

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>To report</b>")
        }

        CheckBox {
          name: "toInterpretation"
          Layout.leftMargin: 450
          text: qsTr("<b>To report</b>")
          id: toInterpretation
        }

    }

    // Expander button for the report phase
    ExpanderButton {
        text: interpretationPhase.expanded ? qsTr("<b>5. Report</b>") : qsTr("5. Report")
        expanded: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? true : false) : false) : false
        enabled: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? true : false) : false) : false
        id: interpretationPhase

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>Download report</b>")
        }
    }
}
