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
        spacing: 70

        ColumnLayout {

            ButtonGroup {
                title: qsTr("Inherent risk")
                name: "IR"

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }

        ColumnLayout {

            ButtonGroup {
                title: qsTr("Control risk")
                name: "CR"

                RadioButton { text: qsTr("High")        ; name: "High" ; checked: true}
                RadioButton { text: qsTr("Medium")      ; name: "Medium" }
                RadioButton { text: qsTr("Low")         ; name: "Low" }
            }
        }
    }

    Divider { }

    GroupBox {

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

        PercentField {
            label.text: qsTr("Expected errors")
            with1Decimal: true
            defaultValue: 2
            name: "k"
        }

    }

    ExpanderButton {
        text: qsTr("Advanced input options")

            ColumnLayout {

                ButtonGroup {
                    title: qsTr("Ratio")
                    name: "show"

                    RadioButton { text: qsTr("Percentages")         ; name: "percentage" ; checked: true}
                    RadioButton { text: qsTr("Proportions")         ; name: "proportion" }
                }
            }

    }

    ColumnLayout {
        GroupBox {
            title: qsTr("Plots")
            CheckBox { text: qsTr("Implied prior") ; name: "plotPriorAndPosterior"               ; id: plotPriorAndPosterior }
            TextField { label.text: qsTr("x-axis limit"); text: "0.2"; name: "limx"; inputType: "number"; Layout.leftMargin: 20; validator: DoubleValidator {bottom: 0; top: 1 } }
            CheckBox { text: qsTr("Additional info")     ; name: "plotPriorAndPosteriorAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: plotPriorAndPosterior.checked}
        }
    }

}
