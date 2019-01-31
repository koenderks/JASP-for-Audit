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
          RadioButton { text: qsTr("Monetary Unit procedure")        ; name: "mus"; id: mus}
        }

        Button {
          anchors.right: parent.right
          text: qsTr("<b>Confirm</b>")
          onClicked: {
            auditType.expanded = false
            sampleFilter.enabled = true
            correctID.enabled = true
            sampleFilterMUS.enabled = true
            correctMUS.enabled = true
            auditOptions.enabled = true
            ir.enabled = true
            cr.enabled = true
            tables.enabled = true
            plots.enabled = true
            boundMethodMUS.enabled = true
            interpretation.enabled = true
            show.enabled = true
            evaluationVariablesAttributes.enabled = true
            evaluationVariablesMUS.enabled = true
            monetaryVariableMUS.enabled = true
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
            id: sampleFilter
            enabled: false
        }
        AssignedVariablesList {
            name: "correctID"
            title: qsTr("Error variable")
            singleItem: true
            allowedColumns: ["nominal"]
            id: correctID
            enabled: false
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
            id: sampleFilterMUS
            enabled: false
        }
        AssignedVariablesList {
            name: "monetaryVariableMUS"
            title: qsTr("Book values")
            singleItem: true
            allowedColumns: ["scale"]
            id: monetaryVariableMUS
            enabled: false
        }
        AssignedVariablesList {
            name: "correctMUS"
            title: qsTr("True values")
            singleItem: true
            allowedColumns: ["scale"]
            id: correctMUS
            enabled: false
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
        text: qsTr("Advanced output options")

        Flow {
            spacing: 70

            ColumnLayout {

                RadioButtonGroup {
                    title: qsTr("<b>Display units</b>")
                    name: "show"
                    id: show
                    enabled: false

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

            GroupBox {
              title: qsTr("<b>Explanatory text</b>")
              id: interpretation
              enabled: false
              CheckBox { text: qsTr("Turn on"); name: "interpretation"; checked: false }
            }

            RadioButtonGroup {
              visible: attributes.checked ? false : true
              title: qsTr("<b>Method</b>")
              name: "boundMethodMUS"
              id: boundMethodMUS
              enabled: false

              RadioButton {
                name: "stringerBound"
                text: qsTr("Stringer")
                checked: true
              }
            }
        }
    }

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
      }

      ColumnLayout {
          GroupBox {
          id: plots
          enabled: false

          title: qsTr("<b>Plots</b>")
                CheckBox { text: qsTr("Confidence bound") ; name: "plotBound"}
          }
      }
    }
}
