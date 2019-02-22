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

    // Variables form for attributes evaluation
    VariablesForm {
      visible: attributes.checked ? true : false
      availableVariablesList.name: "evaluationVariablesAttributes"

        AssignedVariablesList {
            name: "sampleFilter"
            title: qsTr("Sample filter variable")
            singleVariable: true
            allowedColumns: ["nominal"]
        }
        AssignedVariablesList {
            name: "correctID"
            title: qsTr("Error variable")
            singleVariable: true
            allowedColumns: ["nominal"]
        }
    }

    // Variables form for mus evaluation
    VariablesForm {
    visible: attributes.checked ? false : true
    availableVariablesList.name: "evaluationVariablesMUS"

        AssignedVariablesList {
            name: "sampleFilterMUS"
            title: qsTr("Sample filter variable")
            singleVariable: true
            allowedColumns: ["nominal"]
        }
        AssignedVariablesList {
            name: "monetaryVariableMUS"
            title: qsTr("Book values")
            singleVariable: true
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "correctMUS"
            title: qsTr("True values")
            singleVariable: true
            allowedColumns: ["scale"]
        }
    }

    Flow {
        spacing: 60

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

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Inherent risk</b>")
                name: "IR"

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }

        ColumnLayout {

            RadioButtonGroup {
                title: qsTr("<b>Control risk</b>")
                name: "CR"

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }
    }

    RadioButtonGroup{
      name: "auditType"
      title: qsTr("<b>Statement level</b>")
      id: auditProcedure

      RadioButton { text: qsTr("Attributes procedure")           ; name: "attributes" ; checked: true; id: attributes}
      RadioButton { text: qsTr("Monetary Unit procedure")        ; name: "mus"; id: mus}
    }

    ExpanderButton {
        text: qsTr("Advanced output options")

        Flow {
            spacing: 70

            ColumnLayout {

                RadioButtonGroup {
                    title: qsTr("<b>Units</b>")
                    name: "show"

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

            GroupBox {
              title: qsTr("<b>Explanatory text</b>")

              CheckBox { text: qsTr("Turn on"); name: "interpretation"; checked: false }
            }

            RadioButtonGroup {
              visible: attributes.checked ? false : true
              title: qsTr("<b>Method</b>")
              name: "boundMethodMUS"

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
        title: qsTr("<b>Statistics</b>")

        CheckBox {
            text: qsTr("Most Likely Error (MLE)")
            name: "mostLikelyError"
            checked: false
        }
      }

      ColumnLayout {
          GroupBox {

          title: qsTr("<b>Plots</b>")
                CheckBox { text: qsTr("Confidence bound") ; name: "plotBound"}
          }
      }
    }
}
