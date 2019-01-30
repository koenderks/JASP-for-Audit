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
        spacing: 60

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
        spacing: 60

        GroupBox {
            title: qsTr("<b>Observations</b>")

            TextField {
                text: qsTr("Sample size")
                value: "0"
                name: "n"
                inputType: "integer"
                validator: IntValidator { bottom: 0 }
            }

            TextField {
                text: qsTr("Found errors")
                value: "0"
                name: "k"
                inputType: "integer"
                validator: IntValidator { bottom: 0 }
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

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

            GroupBox {
              title: qsTr("<b>Interpretation</b>")
              CheckBox { text: qsTr("Toggle interpretation"); name: "interpretation"; checked: false }
            }

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
                CheckBox { text: qsTr("Confidence bound") ; name: "plotBounds"}
          }
      }

    }

}
