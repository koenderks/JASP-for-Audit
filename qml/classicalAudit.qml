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
    columns: 1

    Section { id: planningPhase; text: planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning"); expanded: true; columns: 1
        GridLayout { columns: 2
            RadioButtonGroup { id: auditType; name: "auditType"; title: qsTr("<b>Materiality</b>")
              RowLayout {
                RadioButton { id: mus; name: "mus"; text: qsTr("Absolute"); checked: true; childrenOnSameRow: true
                  DoubleField { id: materialityValue; visible: mus.checked; name: "materialityValue"; defaultValue: 0; min: 0; fieldWidth: 90; decimals: 2 } }
              }
              RowLayout {
                RadioButton { id: attributes; name: "attributes"; text: qsTr("Relative"); childrenOnSameRow: true
                  PercentField { id: materiality; visible: attributes.checked; decimals: 2; defaultValue: 0; name: "materiality"; fieldWidth: 50 } }
              }
            }
            GroupBox { title: qsTr("<b>Audit risk</b>")
                PercentField { name: "confidence"; label: qsTr("Confidence"); decimals: 2; defaultValue: 95 }
            }
        }
        Divider { }
        Text { text: qsTr("<b>Variable selection</b>"); font.family: "SansSerif"; font.pointSize: 12; Layout.leftMargin: 200 }
        VariablesForm { id: variablesFormPreparation; implicitHeight: 110
            AvailableVariablesList { name: "variablesFormPlanning" }
            AssignedVariablesList { name: "recordNumberVariable"; title: qsTr("Record numbers"); singleVariable: true; allowedColumns: ["nominal", "ordinal", "scale"]; id: recordNumberVariable }
            AssignedVariablesList { name: "monetaryVariable"; title: mus.checked ? qsTr("Book values") : qsTr("Book values (optional)"); singleVariable: true; allowedColumns: ["scale"]; id: monetaryVariable }
        }
        Section { text: qsTr("Advanced planning options")
          GridLayout { columns: 3
              RadioButtonGroup { title: qsTr("<b>Inherent risk</b>"); name: "IR"; id: ir
                    RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                    RadioButton { text: qsTr("Medium") ; name: "Medium" }
                    RadioButton { text: qsTr("Low") ; name: "Low" }
                }
                RadioButtonGroup { name: "expected.errors"; id: expectedErrors; title: qsTr("<b>Expected errors</b>")
                  RowLayout {
                      RadioButton { text: qsTr("Percentage") ; name: "kPercentage" ; checked: true; id: expkPercentage}
                      PercentField { name: "kPercentageNumber"; enabled: expkPercentage.checked; decimals: 2; defaultValue: 0; fieldWidth: 40 }
                  }
                  RowLayout {
                      RadioButton { text: qsTr("Number"); name: "kNumber"; id: expkNumber}
                      IntegerField { name: "kNumberNumber"; enabled: expkNumber.checked; defaultValue: 0; min: 0; max: 9999; fieldWidth: 40; Layout.leftMargin: 18 }
                  }
                }
                GroupBox { title: qsTr("<b>Explanatory text</b>")
                  RowLayout {
                    CheckBox { id: interpretationOn; text: qsTr("Enable"); name: "interpretation"; checked: true }
                    MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                }
                RadioButtonGroup { title: qsTr("<b>Control risk</b>"); name: "CR"; id: cr
                    RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                    RadioButton { text: qsTr("Medium") ; name: "Medium" }
                    RadioButton { text: qsTr("Low") ; name: "Low" }
                }
                RadioButtonGroup {
                    title: qsTr("<b>Sampling model</b>")
                    name: "distribution"
                    id: distribution
                    
                    RadioButton { text: qsTr("Monetary units")                       ; name: "gamma" ; checked: true; id: gamma}
                    RadioButton { text: qsTr("With replacement")                    ; name: "binomial"; id: binomial}
                    RadioButton { text: qsTr("Without replacement")                 ; name: "hypergeometric" ; id: hyperDist}
                }
          }
      }
      Section { title: qsTr("Tables and plots")
        GridLayout { columns: 2  
          GroupBox { title: qsTr("<b>Tables</b>")  
            CheckBox { text: qsTr("Book value descriptives"); name: "descriptivesTable"; enabled: monetaryVariable.count > 0  }  
          }
          GroupBox { title: qsTr("<b>Plots</b>")
            CheckBox { enabled: monetaryVariable.count > 0 ; text: qsTr("Book value distribution"); name: "distributionPlot"; id: distributionPlot }
            CheckBox { text: qsTr("Decision plot"); name: "plotCriticalErrors" }
          }
        }
      }
    Item { height: toSampling.height; Layout.fillWidth: true
      Button { id: downloadReportPlanning; anchors.right: samplingChecked.left; text: qsTr("<b>Download Report</b>")
        enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0)) }
      CheckBox { anchors.right: toSampling.left; width: height; visible: false; name: "samplingChecked"; id: samplingChecked; checked: false }
      Button { id: toSampling; anchors.right: parent.right; text: qsTr("<b>To Selection</b>")
          enabled: attributes.checked ? (materiality.value == "0" ? false : (recordNumberVariable.count > 0)) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0))
          onClicked: {
            planningPhase.expanded = false
            samplingPhase.expanded = true
            samplingPhase.enabled = true
            samplingChecked.checked = true
          }
        }
      }
    }
    Section { text: samplingPhase.expanded ? qsTr("<b>2. Selection</b>") : qsTr("2. Selection"); enabled: false; expanded: false; id: samplingPhase; columns: 1
        VariablesForm { id: variablesFormSampling; implicitHeight: 200
          AvailableVariablesList { name: "variablesFormSampling"}
          AssignedVariablesList { name: "rankingVariable"; title: qsTr("Ranking variable (optional)"); singleVariable: true; allowedColumns: ["scale"] }
          AssignedVariablesList { name: "variables"; title: qsTr("Additional variables (optional)"); singleVariable: false; height: 140; allowedColumns: ["scale", "ordinal", "nominal"] }
        }        
        Section { title: qsTr("Advanced selection options")
              GridLayout { columns: 3
                RadioButtonGroup { title: qsTr("<b>Selection type</b>"); name: "samplingMethod"; id: samplingMethod
                  RowLayout {
                    RadioButton { text: qsTr("Monetary Unit Sampling") ; name: "mussampling" ; id: mussampling; enabled: (monetaryVariable.count > 0 ? true : false); checked: (monetaryVariable.count > 0 ? true : false)}
                    MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with probability proportional to their value"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                  RowLayout {
                    RadioButton { text: qsTr("Record Sampling") ; name: "recordsampling" ; id: recordsampling; enabled: true; checked: (monetaryVariable.count > 0 ? false : true)}
                    MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with equal probability"; radius: 20; Layout.alignment: Qt.AlignRight }
                  }
                }
                RadioButtonGroup { title: qsTr("<b>Selection method</b>"); name: "samplingType"
                  RadioButton { text: qsTr("Random sampling"); name: "simplerandomsampling" ; id: simplerandomsampling}
                  RadioButton { text: qsTr("Cell sampling"); name: "cellsampling" ; id: cellsampling}
                  RadioButton { text: qsTr("Systematic sampling") ; name: "systematicsampling" ; id: systematicsampling; checked: true}
                  IntegerField { text: qsTr("Starting point"); min: 1; Layout.leftMargin: 20; enabled: systematicsampling.checked; fieldWidth: 60; name: "startingPoint"; defaultValue: 1 }
                }      
                IntegerField { text: qsTr("Seed"); name: "seedNumber"; id: seedNumber; defaultValue: 1; min: 1; max: 999; fieldWidth: 60 }
            }
        }    
        Section { title: qsTr("Tables and plots")      
          GridLayout {
              GroupBox { title: qsTr("<b>Tables</b>"); id: samplingTables
                  CheckBox { text: qsTr("Display sample")       ; name: "showSample"}
                      CheckBox { text: qsTr("Sample descriptives")  ; name: "showDescriptives" ; id: descriptives}
                      GridLayout { Layout.leftMargin: 20
                          ColumnLayout { spacing: 5
                            CheckBox { text: qsTr("Mean")                 ; name: "mean"; enabled: descriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Median")               ; name: "median"; enabled: descriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Std. deviation")       ; name: "sd"; enabled: descriptives.checked ; checked: true}
                            CheckBox { text: qsTr("Variance")             ; name: "var"; enabled: descriptives.checked}
                          }
                          ColumnLayout { spacing: 5
                            CheckBox { text: qsTr("Minimum")              ; name: "min"; enabled: descriptives.checked}
                            CheckBox { text: qsTr("Maximum")              ; name: "max"; enabled: descriptives.checked}
                            CheckBox { text: qsTr("Range")                ; name: "range"; enabled: descriptives.checked}
                          }
                      }
              }
          }
        }
        Item { height: toExecution.height; Layout.fillWidth: true
          Button { id: downloadReportSelection; enabled: attributes.checked ? (materiality.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
                  anchors.right: executionChecked.left; text: qsTr("<b>Download Report</b>") }
          CheckBox { anchors.right: toExecution.left; width: height; visible: false; name: "executionChecked"; id: executionChecked; checked: false }
          Button { id: toExecution; anchors.right: parent.right; text: qsTr("<b>To Execution</b>")
                    onClicked: { samplingPhase.expanded = false; executionPhase.expanded = true; executionPhase.enabled = true }
            }
          }
      }
      Section { text: executionPhase.expanded ? qsTr("<b>3. Execution</b>") : qsTr("3. Execution"); expanded: false; enabled: false; id: executionPhase; columns: 1
          Text { text: qsTr("<b>How would you like to evaluate your observations?</b>"); font.family: "SansSerif"; font.pointSize: 10; Layout.leftMargin: 80 }
          RadioButtonGroup { Layout.leftMargin: 50; name: "variableType"; id: variableType; title: qsTr("")
              RowLayout { spacing: 150
                RowLayout {
                  RadioButton { text: qsTr("Audit values") ; name: "variableTypeTrueValues" ; id: variableTypeTrueValues; checked: (monetaryVariable.count > 0 ? true : false); enabled: (monetaryVariable.count > 0 ? true : false) }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Adds a column to specify the audit value of the observations"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
                RowLayout {
                  RadioButton { text: qsTr("Correct / Incorrect") ; name: "variableTypeCorrect" ; id: variableTypeCorrect; checked: (monetaryVariable.count > 0 ? false : true); enabled: true }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip:	"Adds a column to specify the observations as correct (0) or incorrect (1)"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
            }        
            Divider { }
            GroupBox {
              ComputedColumnField { name: "sampleFilterName"; text: "Column name selection result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true }
              ComputedColumnField { name: "variableName"; text: "Column name audit result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true }
            }
            Item { height: toEvaluation.height; Layout.fillWidth: true
              CheckBox { anchors.right: pasteButton.left; width: height; visible: false; name: "pasteVariables"; id: pasteVariables; checked: false }
              Button { text: qsTr("<b>Add Variables</b>"); id: pasteButton; anchors.right: evaluationChecked.left
                onClicked: {
                  toEvaluation.enabled = true; pasteButton.enabled = false; pasteVariables.checked = true; variableType.enabled = false
                  auditType.enabled = false; auditRisk.enabled = false; ir.enabled = false; cr.enabled = false; distribution.enabled = false
                  expectedErrors.enabled = false; variablesFormSampling.enabled = false; seedNumber.enabled = false; samplingType.enabled = false
                  pasteButton.enabled = false; variablesFormPreparation.enabled = false; samplingMethod.enabled = false
                }
              }
              CheckBox { anchors.right: toEvaluation.left; width: height; visible: false; name: "evaluationChecked"; id: evaluationChecked; checked: false }
              Button { enabled: false; id: toEvaluation; anchors.right: parent.right; text: qsTr("<b>To Evaluation</b>")
                onClicked: { executionPhase.expanded = false; evaluationPhase.expanded = true; evaluationPhase.enabled = true; evaluationChecked.checked = true }
              }
            }
        }
        Section { text: evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation"); expanded: false; enabled: false; id: evaluationPhase; columns: 1
            VariablesForm { implicitHeight: 200
              AvailableVariablesList { name: "evaluationVariables"}
              AssignedVariablesList { name: "sampleFilter"; title: qsTr("Selection result"); singleVariable: true; allowedColumns: ["nominal"]; id: sampleFilter }
              AssignedVariablesList { name: "correctID"; title: qsTr("Audit result"); singleVariable: true; allowedColumns: ["nominal" ,"scale"]; id: correctID }
            }
            Section { title: qsTr("Advanced evaluation options"); columns: 1
              GridLayout { columns: 2
                RadioButtonGroup { title: qsTr("<b>Estimator</b>"); name: "boundMethod"
                  RadioButton { name: "stringerBound"; text: qsTr("Stringer"); id: stringerBound; visible: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false
                    enabled: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false; checked: variableTypeTrueValues.checked ? (mussampling.checked ? true : false) : false }
                  RadioButton { name: "gammaBound"; text: qsTr("Gamma"); id: gammaBound; visible: variableTypeCorrect.checked; enabled: variableTypeCorrect.checked
                    checked: variableTypeCorrect.checked ? (gamma.checked ? true : false) : false }
                  RadioButton { name: "binomialBound"; text: qsTr("Binomial"); id: binomialBound; visible: variableTypeCorrect.checked; enabled: variableTypeCorrect.checked
                    checked: variableTypeCorrect.checked ? (binomial.checked ? true : false) : false }
                  RadioButton { name: "hyperBound"; text: qsTr("Hypergeometric"); id: hyperBound; visible: variableTypeCorrect.checked; enabled: variableTypeCorrect.checked
                    checked: variableTypeCorrect.checked ? (binomial.checked ? false : true) : false }
                  RadioButton { name: "regressionBound"; text: qsTr("Regression"); id: regressionBound; visible: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false
                    enabled: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false; checked: variableTypeTrueValues.checked ? (mussampling.checked ? false : true) : false }
                }
              }
            }        
            Section { title: qsTr("Tables and plots")
              GridLayout { columns: 2
                GroupBox { title: qsTr("<b>Statistics</b>")
                  CheckBox { text: qsTr("Most Likely Error (MLE)"); name: "mostLikelyError"; checked: false }
                }
                GroupBox { title: qsTr("<b>Plots</b>")
                    CheckBox { text: qsTr("Evaluation information"); name: "plotBound" }
                    CheckBox { text: qsTr("Correlation plot"); name: "plotCorrelation"; visible: variableTypeTrueValues.checked }
                }
              }
            }
            Item { height: toInterpretation.height; Layout.fillWidth: true
              Button { id: toInterpretation; anchors.right: parent.right; text: qsTr("<b>Download Report</b>")
                enabled: sampleFilter.count > 0 && correctID.count > 0
                onClicked: { evaluationPhase.expanded = false; interpretationPhase.expanded = true; interpretationPhase.enabled = true }
              }
            }
          }
}
