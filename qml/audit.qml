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
        text: qsTr("<b>Planning</b>")
        expanded: toSampling.checked ? false : true
        enabled: toSampling.checked ? false : true

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
            }

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

    Divider { }

    Flow {
        spacing: 40

        RadioButtonGroup {
            title: qsTr("<b>Sampling model</b>")
            name: "distribution"

            RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
            RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
        }

        GroupBox {
          title: qsTr("<b>Allowed errors</b>")

          RadioButtonGroup {
              name: "expected.errors"

              RowLayout {

                  RadioButton { text: qsTr("Percentage")          ; name: "kPercentage" ; checked: true; id: expkPercentage}

                  PercentField {
                      with1Decimal: true
                      defaultValue: 2
                      name: "kPercentageNumber"
                      enabled: expkPercentage.checked
                  }
              }
              RowLayout {

                  RadioButton { text: qsTr("Number")          ; name: "kNumber" ; id: expkNumber}

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

        ColumnLayout {

            GroupBox {
                title: qsTr("<b>Interpretation</b>")

              CheckBox { text: qsTr("Toggle interpretation")     ; name: "interpretation"}
            }
        }
    }

    ExpanderButton {
        text: qsTr("<b>Advanced options</b>")

        Flow {
            spacing: 40

            ColumnLayout {

              RadioButtonGroup {
                  title: qsTr("<b>Inference</b>")
                  name: "inference"

                  RadioButton { text: qsTr("Frequentist")            ; name: "frequentist" ; checked: true}

                  RadioButton { text: qsTr("Bayesian")              ; name: "bayesian"; id: bayesianType}

                  ExpanderButton {
                    text:"<b>Options</b>"
                    enabled: bayesianType.checked
                    implicitWidth: 180

                    GroupBox {
                        CheckBox { text: qsTr("Implicit sample") ; name: "implicitsample"; Layout.leftMargin: 20; enabled: bayesianType.checked}
                        CheckBox { text: qsTr("Implied prior") ; name: "plotPrior" ; id: plotPrior; enabled: bayesianType.checked; Layout.leftMargin: 20}
                        PercentField { text: qsTr("x-axis limit"); defaultValue: 20; name: "limx"; Layout.leftMargin: 40; enabled: bayesianType.checked}
                        CheckBox { text: qsTr("Additional info")     ; name: "plotPriorAdditionalInfo" ; Layout.leftMargin: 40; checked: true; enabled: plotPrior.checked}
                     }

                  }

                }

            }

            ColumnLayout {

                RadioButtonGroup {
                    title: qsTr("<b>Ratio</b>")
                    name: "show"

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

        }

    }

    Button {
      Layout.leftMargin: 450
      text: qsTr("<b>To sampling</b>")
    }
    CheckBox { name: "toSampling"; Layout.leftMargin: 450; text: qsTr("<b>click button</b>"); id: toSampling }

  }

    ExpanderButton {
        text: qsTr("<b>Sampling</b>")
        expanded: toSampling.checked ? (toEvaluation.checked ? false : true) : false
        enabled: toSampling.checked ? (toEvaluation.checked ? true : true) : false

        VariablesForm {
            AssignedVariablesList {
                name: "recordNumberVariable"
                title: qsTr("Record numbers")
                singleItem: true
                allowedColumns: ["nominal"]
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

        Flow {
            spacing: 120

            ColumnLayout {

                GroupBox {
                    title: qsTr("<b>Sampling options</b>")
                    RadioButtonGroup {
                      name: "samplingType"

                      RadioButton { text: qsTr("Simple random sampling")          ; name: "simplerandomsampling" ; id: simplerandomsampling; checked: true}
                      CheckBox { text: qsTr("Allow duplicate records")            ; name: "allowDuplicates"; Layout.leftMargin: 20; enabled: simplerandomsampling.checked }
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
                      RadioButton { text: qsTr("Cell sampling")                   ; name: "cellsampling" ; id: cellsampling}
                    }

                }

                RadioButtonGroup {
                    title: qsTr("<b>Seed</b>")
                    name: "seed"

                    RadioButton { text: qsTr("Default")         ; name: "seedDefault" ; checked: true}
                    RowLayout {

                        RadioButton { text: qsTr("Manual")          ; name: "seedManual" ; id: manualSeed}

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

                    CheckBox { text: qsTr("Sample descriptives") ; name: "showDescriptives" ; id: descriptives}
                    CheckBox { text: qsTr("Mean") ; name: "mean"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Median") ; name: "median"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Std. deviation") ; name: "sd"; Layout.leftMargin: 20; enabled: descriptives.checked ; checked: true}
                    CheckBox { text: qsTr("Variance") ; name: "var"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Minimum") ; name: "min"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Maximum") ; name: "max"; Layout.leftMargin: 20; enabled: descriptives.checked}
                    CheckBox { text: qsTr("Range") ; name: "range"; Layout.leftMargin: 20; enabled: descriptives.checked}
                }

            }

        }

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>To evaluation</b>")
        }
        CheckBox { name: "toEvaluation"; Layout.leftMargin: 450; text: qsTr("<b>click button</b>"); id: toEvaluation }

    }

    ExpanderButton {
        text: qsTr("<b>Evaluation</b>")
        expanded: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? false : true) : false) : false
        enabled: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? false : true) : false) : false

        VariablesForm {
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

        ColumnLayout {
            GroupBox {
                title: qsTr("<b>Plots</b>")
                  CheckBox {
                      text: qsTr("Prior and posterior")
                      name: "plotPriorAndPosterior"
                      id: plotPriorAndPosterior
                      enabled: bayesianType.checked
                  }
                  PercentField {
                    text: qsTr("x-axis limit")
                    defaultValue: 20
                    name: "limx_backup"
                    Layout.leftMargin: 20
                    enabled: bayesianType.checked
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

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>To report</b>")
        }
        CheckBox { name: "toInterpretation"; Layout.leftMargin: 450; text: qsTr("<b>click button</b>"); id: toInterpretation }

    }

    ExpanderButton {
        text: qsTr("<b>Report</b>")
        expanded: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? true : false) : false) : false
        enabled: toSampling.checked ? (toEvaluation.checked ? (toInterpretation.checked ? true : false) : false) : false

        Button {
          Layout.leftMargin: 450
          text: qsTr("<b>Download report</b>")
        }

    }

}
