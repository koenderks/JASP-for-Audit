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
    usesJaspResults: true
    columns: 1

    Section { id: planningPhase; text: planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning"); expanded: true; columns: 1
        GridLayout { columns: 2
            RadioButtonGroup { id: materiality; name: "materiality"; title: qsTr("<b>Materiality</b>")
              RowLayout {
                RadioButton { id: materialityAbsolute; name: "materialityAbsolute"; text: qsTr("Absolute"); checked: true; childrenOnSameRow: true
                  DoubleField { id: materialityValue; visible: materialityAbsolute.checked; name: "materialityValue"; defaultValue: 0; min: 0; fieldWidth: 90; decimals: 2 } }
              }
              RowLayout {
                RadioButton { id: materialityRelative; name: "materialityRelative"; text: qsTr("Relative"); childrenOnSameRow: true
                  PercentField { id: materialityPercentage; visible: materialityRelative.checked; decimals: 2; defaultValue: 0; name: "materialityPercentage"; fieldWidth: 50 } }
              }
            }
            GroupBox { title: qsTr("<b>Audit risk</b>"); id: auditRisk
                PercentField { name: "confidence"; label: qsTr("Confidence"); decimals: 2; defaultValue: 95 }
            }
        }

        Divider { width: parent.width }

        Text { text: qsTr("<b>Variable selection</b>"); font.family: "SansSerif"; font.pointSize: 12; Layout.leftMargin: 220 }
        VariablesForm { id: variablesFormPlanning; implicitHeight: 110
            AvailableVariablesList { name: "variablesFormPlanning" }
            AssignedVariablesList { name: "recordNumberVariable"; title: qsTr("Record numbers"); singleVariable: true; allowedColumns: ["nominal", "ordinal", "scale"]; id: recordNumberVariable }
            AssignedVariablesList { name: "monetaryVariable"; title: materialityAbsolute.checked ? qsTr("Book values") : qsTr("Book values (optional)"); singleVariable: true; allowedColumns: ["scale"]; id: monetaryVariable }
        }

        Section { text: qsTr("Advanced options")
          GridLayout { columns: 3
              RadioButtonGroup { title: qsTr("<b>Inherent risk</b>"); name: "IR"; id: ir
                    RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                    RadioButton { text: qsTr("Medium") ; name: "Medium" }
                    RadioButton { text: qsTr("Low") ; name: "Low" }
                }
                RadioButtonGroup { name: "expectedErrors"; id: expectedErrors; title: qsTr("<b>Expected errors</b>")
                  RowLayout {
                      RadioButton { text: qsTr("Absolute"); name: "expectedAbsolute"; id: expectedAbsolute}
                      DoubleField { name: "expectedNumber"; enabled: expectedAbsolute.checked; defaultValue: 0; min: 0; max: 9999; decimals: 2; visible: expectedAbsolute.checked; fieldWidth: 60 }
                  }
                  RowLayout {
                      RadioButton { text: qsTr("Relative") ; name: "expectedRelative" ; checked: true; id: expectedRelative}
                      PercentField { name: "expectedPercentage"; enabled: expectedRelative.checked; decimals: 2; defaultValue: 0; visible: expectedRelative.checked; fieldWidth: 60  }
                  }
                }
                GroupBox { title: qsTr("<b>Explanatory text</b>")
                  RowLayout {
                    CheckBox { id: explanatoryText; text: qsTr("Enable"); name: "explanatoryText"; checked: true }
                    MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                }
                RadioButtonGroup { title: qsTr("<b>Control risk</b>"); name: "CR"; id: cr
                    RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                    RadioButton { text: qsTr("Medium") ; name: "Medium" }
                    RadioButton { text: qsTr("Low") ; name: "Low" }
                }
                RadioButtonGroup {
                    title: qsTr("<b>Planning distribution</b>")
                    name: "planningModel"
                    id: planningModel

                    RadioButton { text: qsTr("Poisson")         ; name: "Poisson" ; checked: true; id: poisson}
                    RadioButton { text: qsTr("Binomial")        ; name: "binomial"; id: binomial}
                    RadioButton { text: qsTr("Hypergeometric")  ; name: "hypergeometric" ; id: hypergeometric}
                }
          }
      }
      Section { title: qsTr("Tables and plots")
        GridLayout { columns: 2
          GroupBox { title: qsTr("<b>Tables</b>")
            CheckBox { text: qsTr("Book value descriptives"); name: "bookValueDescriptives"; enabled: monetaryVariable.count > 0  }
          }
          GroupBox { title: qsTr("<b>Plots</b>")
            CheckBox { enabled: monetaryVariable.count > 0 ; text: qsTr("Book value distribution"); name: "bookValueDistribution"; id: bookValueDistribution }
            CheckBox { text: qsTr("Decision plot"); name: "decisionPlot" }
          }
        }
      }
    Item { height: toSampling.height; Layout.fillWidth: true
      Button { anchors.left: parent.left; text: qsTr("<b>Reset Workflow</b>");
                onClicked: {
                  form.reset()
                }
              }
      Button { id: downloadReportPlanning; anchors.right: samplingChecked.left; text: qsTr("<b>Download Report</b>")
        enabled: materialityRelative.checked ? (materialityPercentage.value == "0" ? false : true) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0)) }
      CheckBox { anchors.right: toSampling.left; width: height; visible: false; name: "samplingChecked"; id: samplingChecked; checked: false }
      Button { id: toSampling; anchors.right: parent.right; text: qsTr("<b>To Selection</b>")
          enabled: materialityRelative.checked ? (materialityPercentage.value == "0" ? false : (recordNumberVariable.count > 0)) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0))
          onClicked: {
            planningPhase.expanded = false
            samplingPhase.expanded = true
            samplingPhase.enabled = true
            samplingChecked.checked = true
            if (monetaryVariable.count == 0)  recordSampling.click()
            if (monetaryVariable.count > 0)   musSampling.click()
            if (monetaryVariable.count == 0)  variableTypeCorrect.click()
            if (monetaryVariable.count > 0)   variableTypeAuditValues.click()
          }
        }
      }
    }
    Section { text: samplingPhase.expanded ? qsTr("<b>2. Selection</b>") : qsTr("2. Selection"); enabled: false; expanded: false; id: samplingPhase; columns: 1
        VariablesForm { id: variablesFormSampling; implicitHeight: 200
          AvailableVariablesList { name: "variablesFormSampling"}
          AssignedVariablesList { name: "rankingVariable"; title: qsTr("Ranking variable (optional)"); singleVariable: true; allowedColumns: ["scale"] }
          AssignedVariablesList { name: "additionalVariables"; title: qsTr("Additional variables (optional)"); height: 140; allowedColumns: ["scale", "ordinal", "nominal"] }
        }
        Section { title: qsTr("Advanced options")
              GridLayout { columns: 3
                RadioButtonGroup { title: qsTr("<b>Selection type</b>"); name: "selectionType"; id: selectionType
                  RowLayout {
                    RadioButton { text: qsTr("Monetary Unit Sampling"); name: "musSampling"; id: musSampling; enabled: (monetaryVariable.count > 0 ? true : false); checked: true }
                    MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with probability proportional to their value"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                  RowLayout {
                    RadioButton { text: qsTr("Record Sampling") ; name: "recordSampling"; id: recordSampling }
                    MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with equal probability"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                }
                RadioButtonGroup { title: qsTr("<b>Selection method</b>"); name: "selectionMethod"; id: selectionMethod
                  RadioButton { text: qsTr("Random sampling"); name: "randomSampling" ; id: randomSampling}
                  RadioButton { text: qsTr("Cell sampling"); name: "cellSampling" ; id: cellSampling}
                  RadioButton { text: qsTr("Systematic sampling") ; name: "systematicSampling" ; id: systematicSampling; checked: true}
                  IntegerField { text: qsTr("Starting point"); min: 1; Layout.leftMargin: 20; enabled: systematicSampling.checked; fieldWidth: 60; name: "intervalStartingPoint"; defaultValue: 1 }
                }
                IntegerField { text: qsTr("Seed"); name: "seed"; id: seed; defaultValue: 1; min: 1; max: 999; fieldWidth: 60 }
            }
        }
        Section { title: qsTr("Tables and plots")
          GridLayout {
              GroupBox { title: qsTr("<b>Tables</b>"); id: samplingTables
                  CheckBox { text: qsTr("Display sample"); name: "displaySample"}
                      CheckBox { text: qsTr("Sample descriptives"); name: "sampleDescriptives"; id: sampleDescriptives}
                      GridLayout { Layout.leftMargin: 20
                          ColumnLayout { spacing: 5
                            CheckBox { text: qsTr("Mean")                 ; name: "mean"; enabled: sampleDescriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Median")               ; name: "median"; enabled: sampleDescriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Std. deviation")       ; name: "sd"; enabled: sampleDescriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Variance")             ; name: "var"; enabled: sampleDescriptives.checked}
                          }
                          ColumnLayout { spacing: 5
                            CheckBox { text: qsTr("Minimum")              ; name: "min"; enabled: sampleDescriptives.checked}
                            CheckBox { text: qsTr("Maximum")              ; name: "max"; enabled: sampleDescriptives.checked}
                            CheckBox { text: qsTr("Range")                ; name: "range"; enabled: sampleDescriptives.checked}
                          }
                      }
              }
          }
        }
        Item { height: toExecution.height; Layout.fillWidth: true
          Button { anchors.left: parent.left; text: qsTr("<b>Reset Workflow</b>");
                    onClicked: {
                      form.reset()
                    }
                  }
          Button { id: downloadReportSelection; enabled: materialityRelative.checked ? (materialityPercentage.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
                  anchors.right: executionChecked.left; text: qsTr("<b>Download Report</b>") }
          CheckBox { anchors.right: toExecution.left; width: height; visible: false; name: "executionChecked"; id: executionChecked; checked: false }
          Button { id: toExecution; anchors.right: parent.right; text: qsTr("<b>To Execution</b>")
                    onClicked: {
                      samplingPhase.expanded = false
                      executionPhase.expanded = true
                      executionPhase.enabled = true
                      if (monetaryVariable.count == 0)  variableTypeCorrect.click()
                      if (monetaryVariable.count > 0)   variableTypeAuditValues.click()
                  }
            }
          }
      }
      Section { text: executionPhase.expanded ? qsTr("<b>3. Execution</b>") : qsTr("3. Execution"); expanded: false; enabled: false; id: executionPhase; columns: 1
          Text { text: qsTr("<b>How would you like to evaluate your observations?</b>"); font.family: "SansSerif"; font.pointSize: 10; Layout.leftMargin: 80 }
          RadioButtonGroup { Layout.leftMargin: 50; name: "variableType"; id: variableType; title: qsTr("")
              RowLayout { spacing: 150
                RowLayout {
                  RadioButton { text: qsTr("Audit values") ; name: "variableTypeAuditValues" ; id: variableTypeAuditValues; checked: true; enabled: (monetaryVariable.count > 0 ? true : false) }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Adds a column to specify the audit value of the observations"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
                RowLayout {
                  RadioButton { text: qsTr("Correct / Incorrect") ; name: "variableTypeCorrect" ; id: variableTypeCorrect; checked: false; enabled: true }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip:	"Adds a column to specify the observations as correct (0) or incorrect (1)"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
            }

            Divider { width: parent.width }

            GroupBox {
              ComputedColumnField { name: "sampleFilter"; text: "Column name selection result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true }
              AddColumnField { name: "auditResult"; text: "Column name audit result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true }
            }
            
            Text { text: qsTr("<b>Execute the audit before continuing to the evaluation stage</b>"); font.family: "SansSerif"; font.pointSize: 7; Layout.leftMargin: 120; visible: false; id: performAuditText }
            Item { height: toEvaluation.height; Layout.fillWidth: true
              Button { anchors.left: parent.left; text: qsTr("<b>Reset Workflow</b>");
                        onClicked: {
                          form.reset()
                        }
                      }
              CheckBox { anchors.right: pasteButton.left; width: height; visible: false; name: "pasteVariables"; id: pasteVariables; checked: false }
              Button { text: qsTr("<b>Add Variables</b>"); id: pasteButton; anchors.right: evaluationChecked.left
                onClicked: {
                  focus = true
                  toEvaluation.enabled = true
                  pasteButton.enabled = false
                  pasteVariables.checked = true
                  variableType.enabled = false
                  materiality.enabled = false
                  auditRisk.enabled = false
                  ir.enabled = false
                  cr.enabled = false
                  planningModel.enabled = false
                  expectedErrors.enabled = false
                  variablesFormSampling.enabled = false
                  seed.enabled = false
                  selectionType.enabled = false
                  pasteButton.enabled = false
                  variablesFormPlanning.enabled = false
                  selectionMethod.enabled = false
                  performAuditText.visible = true
                }
              }
              CheckBox { anchors.right: toEvaluation.left; width: height; visible: false; name: "evaluationChecked"; id: evaluationChecked; checked: false }
              Button { enabled: false; id: toEvaluation; anchors.right: parent.right; text: qsTr("<b>To Evaluation</b>")
                onClicked: {
                  executionPhase.expanded = false
                  evaluationPhase.expanded = true
                  evaluationPhase.enabled = true
                  evaluationChecked.checked = true
                  if (musSampling.checked & variableTypeAuditValues.checked) stringerBound.click()
                  if (musSampling.checked & variableTypeAuditValues.checked) stringerBound.visible = true
                  if (recordSampling.checked & variableTypeAuditValues.checked) regressionBound.click()
                  if (recordSampling.checked & variableTypeAuditValues.checked) directBound.visible = true
                  if (recordSampling.checked & variableTypeAuditValues.checked) differenceBound.visible = true
                  if (recordSampling.checked & variableTypeAuditValues.checked) ratioBound.visible = true
                  if (recordSampling.checked & variableTypeAuditValues.checked) regressionBound.visible = true
                  if (variableTypeCorrect.checked) gammaBound.click()
                  if (variableTypeCorrect.checked) gammaBound.visible = true
                  if (variableTypeCorrect.checked) binomialBound.visible = true
                  if (variableTypeCorrect.checked) hyperBound.visible = true
                }
              }
            }
        }
        Section { text: evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation"); expanded: false; enabled: false; id: evaluationPhase; columns: 1
            
            /* commented out because you can just use the value of the added columns!
            VariablesForm { implicitHeight: 200
              AvailableVariablesList { name: "evaluationVariables"}
              AssignedVariablesList { name: "sampleFilter"; title: qsTr("Selection result"); singleVariable: true; allowedColumns: ["nominal"]; id: sampleFilter }
              AssignedVariablesList { name: "auditResult"; title: qsTr("Audit result"); singleVariable: true; allowedColumns: ["nominal" ,"scale"]; id: auditResult }
            }*/
            Section { title: qsTr("Advanced options"); columns: 1
              GridLayout { columns: 2
                RadioButtonGroup { title: qsTr("<b>Estimator</b>"); name: "estimator"
                  RadioButton { name: "stringerBound"; text: qsTr("Stringer"); id: stringerBound; visible: false }
                  RadioButton { name: "directBound"; text: qsTr("Direct"); id: directBound; visible: false }
                  RadioButton { name: "differenceBound"; text: qsTr("Difference"); id: differenceBound; visible: false }
                  RadioButton { name: "ratioBound"; text: qsTr("Ratio"); id: ratioBound; visible: false }
                  RadioButton { name: "regressionBound"; text: qsTr("Regression"); id: regressionBound; visible: false }
                  RadioButton { name: "gammaBound"; text: qsTr("Gamma"); id: gammaBound; visible: false }
                  RadioButton { name: "binomialBound"; text: qsTr("Binomial"); id: binomialBound; visible: false }
                  RadioButton { name: "hyperBound"; text: qsTr("Hypergeometric"); id: hyperBound; visible: false }
                }
              }
            }
            Section { title: qsTr("Tables and plots")
              GridLayout { columns: 2
                GroupBox { title: qsTr("<b>Statistics</b>")
                  CheckBox { text: qsTr("Most Likely Error (MLE)"); name: "mostLikelyError"; checked: false }
                }
                GroupBox { title: qsTr("<b>Plots</b>")
                    CheckBox { text: qsTr("Evaluation information"); name: "evaluationInformation" }
                    CheckBox { text: qsTr("Correlation plot"); name: "correlationPlot"; visible: variableTypeAuditValues.checked }
                }
              }
            }
            Item { height: toInterpretation.height; Layout.fillWidth: true
              Button { id: toInterpretation; anchors.right: parent.right; text: qsTr("<b>Download Report</b>")
                enabled: sampleFilter.count > 0 && auditResult.count > 0
                onClicked: { evaluationPhase.expanded = false }
              }
            }
          }
}
