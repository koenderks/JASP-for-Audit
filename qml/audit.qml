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
        spacing: 90

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
        spacing: 80

        GroupBox {
          title: qsTr("<b>Expected errors</b>")

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

        RadioButtonGroup {
            title: qsTr("<b>Sampling plan</b>")
            name: "distribution"

            RadioButton { text: qsTr("With replacement")            ; name: "binomial" ; checked: true}
            RadioButton { text: qsTr("Without replacement")         ; name: "hypergeometric" ; id: hyperDist}
        }


    }

    ExpanderButton {
        text: qsTr("<b>Advanced options</b>")

        Flow {
            spacing: 90

            ColumnLayout {

              RadioButtonGroup {
                  title: qsTr("<b>Inference</b>")
                  name: "inference"

                  RadioButton { text: qsTr("Frequentist")            ; name: "frequentist" ; checked: true}

                  RadioButton { text: qsTr("Bayesian")              ; name: "bayesian"; id: bayesianType}

                    ExpanderButton {
                      text:"<b>Options</b>"

                      GroupBox {
                          CheckBox { text: qsTr("Implicit sample") ; name: "implicitsample"; Layout.leftMargin: 20; enabled: bayesianType.checked}
                          CheckBox { text: qsTr("Implied prior") ; name: "plotPriorAndPosterior" ; id: plotPriorAndPosterior; enabled: bayesianType.checked; Layout.leftMargin: 20}
                          PercentField { text: qsTr("x-axis limit"); defaultValue: 20; name: "limx"; Layout.leftMargin: 40; enabled: bayesianType.checked}
                          CheckBox { text: qsTr("Additional info")     ; name: "plotPriorAndPosteriorAdditionalInfo" ; Layout.leftMargin: 40; checked: true; enabled: plotPriorAndPosterior.checked}
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

            ColumnLayout {

                GroupBox {
                    title: qsTr("<b>Interpretation</b>")

                  CheckBox { text: qsTr("Toggle interpretation")     ; name: "interpretation"}
                }
            }

        }
    }

    Button {
      Layout.leftMargin: 400
      text: qsTr("<b>To sampling</b>")
    }

}
