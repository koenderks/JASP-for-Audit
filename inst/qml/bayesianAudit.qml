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

    Section { id: planningPhase; text: planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning"); columns: 1; expanded: true
      GridLayout { columns: 2
          RadioButtonGroup { id: materiality; name: "materiality"; title: qsTr("Population materiality")
            RowLayout {
              RadioButton { id: materialityAbsolute; name: "materialityAbsolute"; text: qsTr("Absolute"); checked: true; childrenOnSameRow: true
                DoubleField { id: materialityValue; visible: materialityAbsolute.checked; name: "materialityValue"; defaultValue: 0; min: 0; fieldWidth: 90; decimals: 2; label: euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value) } }
            }
            RowLayout {
              RadioButton { id: materialityRelative; name: "materialityRelative"; text: qsTr("Relative"); childrenOnSameRow: true
                PercentField { id: materialityPercentage; visible: materialityRelative.checked; decimals: 2; defaultValue: 0; name: "materialityPercentage"; fieldWidth: 40 } }
            }
          }
          GroupBox { title: qsTr("Audit risk"); id: auditRisk
              PercentField { name: "confidence"; label: qsTr("Confidence"); decimals: 2; defaultValue: 95 }
          }
      }

      Divider { width: parent.width }

      Item {
        height: variableSelectionTitle.height
        Layout.fillWidth: true
        Text { id: variableSelectionTitle; anchors.horizontalCenter: parent.horizontalCenter
            text: qsTr("<b>Variable definition</b>"); font.family: "SansSerif"; font.pointSize: 12
        }
      }
      VariablesForm { id: variablesFormPlanning; implicitHeight: 110
          AvailableVariablesList { name: "variablesFormPlanning" }
          AssignedVariablesList { name: "recordNumberVariable"; title: qsTr("Record ID's"); singleVariable: true; allowedColumns: ["nominal", "ordinal", "scale"]; id: recordNumberVariable }
          AssignedVariablesList { name: "monetaryVariable"; title: materialityAbsolute.checked ? qsTr("Book values <i>(required)</i>") : qsTr("Book values <i>(recommended)</i>"); singleVariable: true; allowedColumns: ["scale"]; id: monetaryVariable }
      }

      Section { text: qsTr("Advanced options")
        GridLayout { columns: 3
            RadioButtonGroup { title: qsTr("Inherent risk"); name: "IR"; id: ir
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup { name: "expectedErrors"; id: expectedErrors; title: qsTr("Expected errors")
                RowLayout {
                    RadioButton { text: qsTr("Absolute"); name: "expectedAbsolute"; id: expectedAbsolute}
                    DoubleField { name: "expectedNumber"; enabled: expectedAbsolute.checked; defaultValue: 0; min: 0; max: 1e10; decimals: 2; visible: expectedAbsolute.checked; fieldWidth: 60; label: euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value) }
                }
                RowLayout {
                    RadioButton { text: qsTr("Relative") ; name: "expectedRelative"; id: expectedRelative; checked: true}
                    PercentField { name: "expectedPercentage"; enabled: expectedRelative.checked; decimals: 3; defaultValue: 0; visible: expectedRelative.checked; fieldWidth: 40  }
                }
              }
              GroupBox { title: qsTr("Explanatory text")
                RowLayout {
                  CheckBox { id: explanatoryText; text: qsTr("Enable"); name: "explanatoryText"; checked: true }
                  MenuButton { width:	20; iconSource: "qrc:/images/info-button.png"; toolTip: "Show explanatory text at each step of the analysis"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
              RadioButtonGroup { title: qsTr("Control risk"); name: "CR"; id: cr
                  RadioButton { text: qsTr("High") ; name: "High" ; checked: true}
                  RadioButton { text: qsTr("Medium") ; name: "Medium" }
                  RadioButton { text: qsTr("Low") ; name: "Low" }
              }
              RadioButtonGroup { title: qsTr("Planning distribution"); name: "planningModel"; id: planningModel
                  RadioButton { text: qsTr("Beta")                          ; name: "beta" ; checked: true; id: beta}
                  RadioButton { text: qsTr("Beta-binomial")                 ; name: "beta-binomial"; id: betaBinomial}
              }
              RadioButtonGroup {
                title: qsTr("Currency")
                name: "valuta"
                id: valuta

                RadioButton { text: qsTr("Euro (€)"); name: "euroValuta"; checked: true; id: euroValuta }
                RadioButton { text: qsTr("Dollar ($)"); name: "dollarValuta"; checked: false; id: dollarValuta }
                RowLayout {
                  RadioButton { text: qsTr("Other"); name: "otherValuta"; checked: false; id: otherValuta}
                  TextField { name: "otherValutaName"; fieldWidth: 40; id: otherValutaName; enabled: otherValuta.checked; visible: otherValuta.checked }
                }
              }
        }
    }
    Section { title: qsTr("Tables and plots")
      GridLayout { columns: 2
          ColumnLayout {
              GroupBox { title: qsTr("Statistics")
                CheckBox { text: qsTr("Expected Bayes factor\u208B\u208A") ; name: "expectedBF"}
              }
              GroupBox { title: qsTr("Tables")
                CheckBox { text: qsTr("Book value descriptives"); name: "bookValueDescriptives"; enabled: monetaryVariable.count > 0}
                CheckBox { text: qsTr("Implicit sample") ; name: "implicitSampleTable"}
              }
          }
          GroupBox { title: qsTr("Plots")
            CheckBox { enabled: monetaryVariable.count > 0 ; text: qsTr("Book value distribution"); name: "bookValueDistribution"; id: bookValueDistribution }
            CheckBox { text: qsTr("Decision analysis"); name: "decisionPlot" }
            CheckBox { text: qsTr("Implied prior from risk assessments"); name: "priorPlot"; id: priorPlot }
            PercentField { text: qsTr("x-axis limit") ; name: "priorPlotLimit" ; defaultValue: 100; Layout.leftMargin: 20; enabled: priorPlot.checked }
            CheckBox { text: qsTr("Additional info") ; name: "priorPlotAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: priorPlot.checked }
            CheckBox { text: qsTr("Expected posterior") ; name: "priorPlotExpectedPosterior" ; Layout.leftMargin: 20; checked: false; enabled: priorPlot.checked }
          }
      }
  }
  Item { height: toSampling.height; Layout.fillWidth: true
      Button { id: downloadReportPlanning; anchors.right: samplingChecked.left; text: qsTr("<b>Download report</b>")
          enabled: materialityRelative.checked ? (materialityPercentage.value == "0" ? false : true) : (materialityValue.value == "0" ? false : (recordNumberVariable.count > 0 && monetaryVariable.count > 0)) 
          onClicked: {
            form.exportResults()
          }
        }
        CheckBox { anchors.right: toSampling.left; width: height; visible: false; name: "samplingChecked"; id: samplingChecked; checked: false }
        Button { id: toSampling; anchors.right: parent.right; text: qsTr("<b>To selection</b>")
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
        AvailableVariablesList { name: "variablesFormSampling"; source: "variablesFormPlanning"}
        AssignedVariablesList { name: "rankingVariable"; title: qsTr("Ranking variable <i>(optional)</i>"); singleVariable: true; allowedColumns: ["scale"] }
        AssignedVariablesList { name: "additionalVariables"; title: qsTr("Additional variables <i>(optional)</i>"); height: 140; allowedColumns: ["scale", "ordinal", "nominal"] }
      }
      Section { title: qsTr("Advanced options")
            GridLayout { columns: 3
              RadioButtonGroup { title: qsTr("Sampling units"); name: "selectionType"; id: selectionType
                RowLayout {
                  RadioButton { text: qsTr("Monetary unit sampling") ; name: "musSampling" ; id: musSampling; enabled: (monetaryVariable.count > 0 ? true : false); checked: true }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with probability proportional to their value"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
                RowLayout {
                  RadioButton { text: qsTr("Record sampling") ; name: "recordSampling" ; id: recordSampling }
                  MenuButton { width: 20; iconSource: "qrc:/images/info-button.png"; toolTip: "Select observations with equal probability"; radius: 20; Layout.alignment: Qt.AlignRight }
                }
              }
              RadioButtonGroup { title: qsTr("Selection method"); name: "selectionMethod"; id: selectionMethod
                RadioButton { text: qsTr("Random sampling"); name: "randomSampling" ; id: randomSampling}
                RadioButton { text: qsTr("Cell sampling"); name: "cellSampling" ; id: cellSampling}
                RadioButton { text: qsTr("Fixed interval sampling") ; name: "systematicSampling" ; id: systematicSampling; checked: true}
              }
              IntegerField { text: qsTr("Seed"); name: "seed"; id: seed; defaultValue: 1; min: 1; max: 999; fieldWidth: 60 }
          }
      }
      Section { title: qsTr("Tables and plots")
        GridLayout {
            GroupBox { title: qsTr("Tables"); id: samplingTables
                CheckBox { text: qsTr("Display selected observations")       ; name: "displaySample"}
                    CheckBox { text: qsTr("Selection descriptives")  ; name: "sampleDescriptives" ; id: sampleDescriptives}
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
        Button { anchors.left: parent.left; text: qsTr("<b>Reset workflow</b>");
                  onClicked: {
                    form.reset()
                  }
                }
        Button { id: downloadReportSelection; enabled: materialityRelative.checked ? (materialityPercentage.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
                anchors.right: executionChecked.left; text: qsTr("<b>Download report</b>") 
                onClicked: {
                  form.exportResults()
                }
              }
        CheckBox { anchors.right: toExecution.left; width: height; visible: false; name: "executionChecked"; id: executionChecked; checked: false }
        Button { id: toExecution; anchors.right: parent.right; text: qsTr("<b>To execution</b>")
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
        Item { height: selectHowToAnalyseObservations.height; Layout.fillWidth: true
          Text { id: selectHowToAnalyseObservations; anchors.horizontalCenter: parent.horizontalCenter
            text: qsTr("<b>How would you like to evaluate your observations?</b>"); font.family: "SansSerif"; font.pointSize: 10
          }
        }
        Item { height: variableType.height; Layout.fillWidth: true
          RadioButtonGroup { name: "variableType"; id: variableType; title: qsTr(""); anchors.horizontalCenter: parent.horizontalCenter
              RowLayout { spacing: 200
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
        }

          Divider { width: parent.width }

          RowLayout{
            GroupBox { id: groupBoxVariableNames
              AddColumnField { name: "sampleFilter"; text: "Column name selection result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true; id: sampleFilter }
              AddColumnField { name: "variableName"; text: "Column name audit result: "; fieldWidth: 120; enabled: pasteVariables.checked ? false : true; id: variableName }
            }
            Item { height: groupBoxVariableNames.height; Layout.fillWidth: true; id: hoi
              CheckBox { anchors.right: pasteButton.left; width: height; visible: false; name: "pasteVariables"; id: pasteVariables; checked: false }
              Button { text: qsTr("<b>Add variables</b>"); id: pasteButton; anchors.right: parent.parent.right; enabled: (sampleFilter.value != "" & variableName.value != "")
                onClicked: {
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
            }
          }
          Item { height: performAuditText.height; Layout.fillWidth: true
            Text { id: performAuditText; anchors.horizontalCenter: parent.horizontalCenter
              text: qsTr("<b>Execute the audit before continuing to the evaluation stage.</b>"); font.family: "SansSerif"; font.pointSize: 7; visible: false
            }
          }
            Item { height: toEvaluation.height; Layout.fillWidth: true
            Button { anchors.left: parent.left; text: qsTr("<b>Reset workflow</b>");
                      onClicked: {
                        form.reset()
                      }
                    }
            CheckBox { anchors.right: toEvaluation.left; width: height; visible: false; name: "evaluationChecked"; id: evaluationChecked; checked: false }
            Button { enabled: false; id: toEvaluation; anchors.right: parent.right; text: qsTr("<b>To evaluation</b>")
              onClicked: {
                executionPhase.expanded = false
                evaluationPhase.expanded = true
                evaluationPhase.enabled = true
                evaluationChecked.checked = true
                if (musSampling.checked & variableTypeAuditValues.checked)    coxAndSnellBound.click()
                if (musSampling.checked & variableTypeAuditValues.checked)    coxAndSnellBound.visible = true
                if (recordSampling.checked & variableTypeAuditValues.checked) regressionBound.click()
                if (recordSampling.checked & variableTypeAuditValues.checked) regressionBound.visible = true
                if (variableTypeCorrect.checked & beta.checked) betaBound.click()
                if (variableTypeCorrect.checked) betaBound.visible = true
                if (variableTypeCorrect.checked & betaBinomial.checked) betabinomialBound.click()
                if (variableTypeCorrect.checked) betabinomialBound.visible = true
              }
            }
          }
      }
      Section { text: evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation"); expanded: false; enabled: false; id: evaluationPhase; columns: 1
        VariablesForm { implicitHeight: 150
          AvailableVariablesList { name: "evaluationVariables"; source: "variablesFormPlanning"}
          AssignedVariablesList { name: "auditResult"; title: qsTr("Audit result"); singleVariable: true; allowedColumns: ["nominal" ,"scale"] ; id: auditResult }
        }
        Section { title: qsTr("Advanced options"); columns: 1
          GridLayout { columns: 2
            RadioButtonGroup { title: qsTr("Estimation method"); name: "estimator"
              RadioButton { name: "coxAndSnellBound"; text: qsTr("Cox and Snell"); id: coxAndSnellBound; visible: false }
              RadioButton { name: "betaBound"; text: qsTr("Beta"); id: betaBound; visible: false }
              RadioButton { name: "betabinomialBound"; text: qsTr("Beta-binomial"); id: betabinomialBound; visible: false }
              RadioButton { name: "regressionBound"; text: qsTr("Regression"); id: regressionBound; visible: false }
            }

            RadioButtonGroup { title: qsTr("Area under posterior"); name: "areaUnderPosterior" 
                RadioButton { text: qsTr("Credible bound"); name: "displayCredibleBound" }
                RadioButton { text: qsTr("Credible interval"); name: "displayCredibleInterval" }
              }
          }
        }
        Section { title: qsTr("Tables and plots")
          GridLayout { columns: 2
            GroupBox { title: qsTr("Statistics")
              CheckBox { text: qsTr("Most likely error (MLE)"); name: "mostLikelyError" }
              CheckBox { text: qsTr("Bayes factor\u208B\u208A"); name: "bayesFactor" }
            }
            GroupBox { title: qsTr("Plots")
              CheckBox { text: qsTr("Evaluation information"); name: "evaluationInformation" }
              CheckBox { text: qsTr("Prior and posterior"); name: "priorAndPosteriorPlot"; id: priorAndPosteriorPlot }
              PercentField { text: qsTr("x-axis limit"); defaultValue: 20; name: "priorAndPosteriorPlotLimit"; Layout.leftMargin: 20 }
              CheckBox { text: qsTr("Additional info"); name: "priorAndPosteriorPlotAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: priorAndPosteriorPlot.checked }
              CheckBox { text: qsTr("Correlation plot"); name: "correlationPlot"; visible: variableTypeAuditValues.checked }
            }
          }
        }
        Item { height: toInterpretation.height; Layout.fillWidth: true
          Button { id: toInterpretation; anchors.right: parent.right; text: qsTr("<b>Download report</b>")
            enabled: auditResult.count > 0
            onClicked: { evaluationPhase.expanded = false; form.exportResults() }
          }
        }
    }
}
