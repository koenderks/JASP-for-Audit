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

    ExpanderButton {
        text: qsTr("<b>Planning</b>")
        expanded: true

        Flow {
            spacing: 40

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

            }
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

             GroupBox {
                 title: qsTr("<b>Prior distribution</b>")

                CheckBox { text: qsTr("Show corresponding prior") ; name: "plotPrior" ; checked: false}
                PercentField {
                  label.text: qsTr("x-axis limit")
                  with1Decimal: false
                  defaultValue: 100
                  name: "limx"
                  }
            }
        }

    }

    ExpanderButton {
        text: qsTr("<b>Evaluation</b>")
        expanded: false

        Flow {
            spacing: 40

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
    GroupBox {
        title: qsTr("<b>Posterior distribution</b>")
        CheckBox { text: qsTr("Show corresponding posterior") ; name: "plotPriorAndPosterior" ; checked: false}
    }

    }

    }

}
