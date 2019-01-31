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
            title: samplingMethod.expanded ? qsTr("<b>Sampling method</b>") : qsTr("Sampling method")
            id: samplingMethod

            RadioButtonGroup{
              name: "auditType"
              title: qsTr("<b>Statement level</b>")
              id: auditProcedure

              RadioButton { text: qsTr("Percentages")           ; name: "attributes" ; checked: true; id: attributes}
              RadioButton { text: qsTr("Monetary Units")        ; name: "mus"; id: mus}
            }

            RowLayout {
              Label {
                text: qsTr("<b>Random</b>")
                }
              Divider {
                implicitWidth: 400
              }
              Label {
                text: qsTr("<b>Systematic</b>")
                }
            }

            RadioButtonGroup {
              name: "samplingType"
              Layout.leftMargin: 20
              id: samplingType

              Flow {
                spacing: 15

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
              anchors.right: parent.right
              text: qsTr("<b>Confirm</b>")
              onClicked: {
                samplingMethod.expanded = false
                variablesFormAttributesSampling.enabled = true
                variablesFormMUSSampling.enabled = true
                seed.enabled = true
                samplingTables.enabled = true
                sampleSize.enabled = true
              }
            }
        }

        // Variables form for attributes sampling
        VariablesForm {
            availableVariablesList.name: "allAvailableVariablesAttributesSampling"
            id: variablesFormAttributesSampling
            visible: attributes.checked ? true : false
            enabled: false

            AssignedVariablesList {
                name: "recordNumberVariable"
                title: qsTr("Record numbers")
                singleItem: true
                allowedColumns: ["nominal", "ordinal", "scale"]
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
            availableVariablesList.name: "allAvailableVariablesMUSSampling"
            id: variablesFormMUSSampling
            visible: attributes.checked ? false : true
            enabled: false

            AssignedVariablesList {
                name: "recordNumberVariableMUS"
                title: qsTr("Record numbers")
                singleItem: true
                allowedColumns: ["nominal", "ordinal", "scale"]
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
            spacing: 120

            ColumnLayout {

            GroupBox {
              title: qsTr("<b>Sample size</b>")
              id: sampleSize
              enabled: false

              TextField {
                text: qsTr("Number of records to select")
                value: "0"
                name: "sampleSize"
                inputType: "integer"
                validator: IntValidator { bottom: 1 }
                Layout.leftMargin: 20
              }
            }

                RadioButtonGroup {
                    title: qsTr("<b>Seed</b>")
                    name: "seed"
                    id: seed
                    enabled: false

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
                    id: samplingTables
                    enabled: false

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
}
