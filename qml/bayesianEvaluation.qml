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

    ExpanderButton {
        expanded: true
        title: auditType.expanded ? qsTr("<b>Audit Type</b>") : qsTr("Audit Type")
        id: auditType

        RadioButtonGroup{
          name: "auditType"
          title: qsTr("<b>Audit procedure</b>")
          id: auditProcedure

          RadioButton { text: qsTr("Attributes procedure")           ; name: "attributes" ; checked: true; id: attributes}
          RadioButton { text: qsTr("Monetary Unit procedure")          ; name: "mus"; id: mus}
        }

        Button {
          anchors.right: parent.right
          text: qsTr("<b>Confirm</b>")
          onClicked: {
            auditType.expanded = false
            evaluationVariablesAttributes.enabled = true
            evaluationVariablesMUS.enabled = true
            auditOptions.enabled = true
            advancedPriorOptions.enabled = true
            ir.enabled = true
            cr.enabled = true
            tables.enabled = true
            plots.enabled = true
            expectedErrors.enabled = true
            distribution.enabled = true
            advancedOutputOptions.enabled = true
          }
        }
    }

    // Variables form for attributes evaluation
    VariablesForm {
      visible: attributes.checked ? true : false
      availableVariablesList.name: "evaluationVariablesAttributes"
      id: evaluationVariablesAttributes
      enabled: false

        AssignedVariablesList {
            name: "sampleFilter"
            title: qsTr("Sample filter variable")
            singleItem: true
            allowedColumns: ["nominal"]
        }
        AssignedVariablesList {
            name: "correctID"
            title: qsTr("Error variable")
            singleItem: true
            allowedColumns: ["nominal"]
        }
    }

    // Variables form for mus evaluation
    VariablesForm {
    visible: attributes.checked ? false : true
    availableVariablesList.name: "evaluationVariablesMUS"
    id: evaluationVariablesMUS
    enabled: false

        AssignedVariablesList {
            name: "sampleFilterMUS"
            title: qsTr("Sample filter variable")
            singleItem: true
            allowedColumns: ["nominal"]
        }
        AssignedVariablesList {
            name: "monetaryVariableMUS"
            title: qsTr("Book values")
            singleItem: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "correctMUS"
            title: qsTr("True values")
            singleItem: true
            allowedColumns: ["scale"]
        }
    }

    Flow {
        spacing: 60

          GroupBox {
              title: qsTr("<b>Audit risk</b>")
              id: auditOptions
              enabled: false

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

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Inherent risk</b>")
                name: "IR"
                id: ir
                enabled: false

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Control risk</b>")
                name: "CR"
                id: cr
                enabled: false

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced prior options")
        id: advancedPriorOptions
        enabled: false

        RadioButtonGroup {
            title: qsTr("<b>Prior</b>")
            name: "prior"

            RadioButton { text: qsTr("Audit Risk Model")        ; name: "ARM" ; checked: true}
            RadioButton { text: qsTr("50-50")                   ; name: "5050" }
        }

    }

    Divider { }

    Flow {
        spacing: 60

        RadioButtonGroup {
            title: qsTr("<b>Sampling model</b>")
            name: "distribution"
            id: distribution
            enabled: false

            RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
            RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
        }

        GroupBox {
          title: qsTr("<b>Allowed errors</b>")
          id: expectedErrors
          enabled: false

          RadioButtonGroup {
              name: "expected.errors"

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
    }

    Divider { }

    Flow{
      spacing: 100

      GroupBox {
        title: qsTr("<b>Tables</b>")
        id: tables
        enabled: false

        CheckBox {
            text: qsTr("Most likely error")
            name: "mostLikelyError"
            checked: false
        }
        CheckBox {
            text: qsTr("Bayes factor")
            name: "bayesFactor"
        }

      }

      ColumnLayout {
          GroupBox {
              title: qsTr("<b>Plots</b>")
              id: plots
              enabled: false
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

    ExpanderButton {
        text: qsTr("Advanced output options")
        enabled: false
        id: advancedOutputOptions

        Flow {
            spacing: 70

            ColumnLayout {

                RadioButtonGroup {
                    title: qsTr("<b>Display units</b>")
                    name: "show"

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

            ColumnLayout {
                RadioButtonGroup {
                    title: qsTr("<b>Statistic</b>")
                    name: "statistic"

                    RadioButton { text: qsTr("Confidence bound")        ; name: "bound" ; checked: true}
                    RadioButton { text: qsTr("Confidence interval")      ; name: "interval" }
                }
            }

            GroupBox {
              title: qsTr("<b>Explanatory text</b>")
              CheckBox { text: qsTr("Turn on"); name: "interpretation"; checked: false }
            }
        }
        RadioButtonGroup {
          visible: attributes.checked ? false : true
          title: qsTr("<b>Method</b>")
          name: "boundMethodMUS"

          RadioButton {
            name: "coxAndSnellBound"
            text: qsTr("Cox and Snell")
            checked: true
          }
        }
    }
}
