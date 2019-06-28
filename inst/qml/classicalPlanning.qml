
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
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

	GridLayout 
	{
		columns: 3

		RadioButtonGroup 
		{
			id: 	materiality
			name: 	"materiality"
			title: 	qsTr("Population Materiality")

			RowLayout 
			{
				RadioButton 
				{
					id: 				materialityAbsolute
					name: 				"materialityAbsolute"
					text: 				qsTr("Absolute")
					checked: 			true
					childrenOnSameRow: 	true

					DoubleField 
					{
						id: 			materialityValue
						visible: 		materialityAbsolute.checked
						name: 			"materialityValue"
						defaultValue: 	0
						min: 			0
						fieldWidth: 	90
						decimals: 		2
						label: 			euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)
					}
				}
			}

			RowLayout 
			{
				RadioButton 
				{
					id: 					materialityRelative
					name: 					"materialityRelative"
					text: 					qsTr("Relative")
					childrenOnSameRow: 		true

					PercentField 
					{
						id: 				materialityPercentage
						visible: 			materialityRelative.checked
						decimals: 			2
						defaultValue: 		0
						name: 				"materialityPercentage"
						fieldWidth: 		40
					}
				}
			}
		}

		GroupBox 
		{
			title: qsTr("Population")

			IntegerField 
			{
				id: 			populationSize
				name: 			"populationSize"
				text: 			qsTr("Size")
				fieldWidth: 	100
				defaultValue: 	0
				min: 			0
			}

			DoubleField 
			{
				id: 			populationValue
				name: 			"populationValue"
				text: 			qsTr("Value")
				defaultValue: 	0
				enabled: 		materialityAbsolute.checked
				fieldWidth: 	100
				min: 			0
				decimals: 		2
			}
		}

		GroupBox 
		{
			id: 		auditRisk
			title: 		qsTr("Audit Risk")

			PercentField 
			{
				name: 			"confidence"
				label: 			qsTr("Confidence")
				decimals: 		2
				defaultValue: 	95
			}
		}
	}

	Section 
	{
		text: qsTr("Advanced Options")

		GridLayout 
		{
			columns: 3

			RadioButtonGroup 
			{
				id: 		ir
				title: 		qsTr("Inherent Risk")
				name: 		"IR"

				RadioButton 
				{
					text: 		qsTr("High")
					name: 		"High"
					checked: 	true
				}

				RadioButton 
				{
					text: 		qsTr("Medium")
					name: 		"Medium"
				}

				RadioButton 
				{
					text:	 	qsTr("Low")
					name: 		"Low"
				}
			}

			RadioButtonGroup 
			{
				id: 		expectedErrors
				name: 		"expectedErrors"
				title: 		qsTr("Expected Errors")

				RowLayout 
				{
					enabled: materialityAbsolute.checked

					RadioButton 
					{
						id: 	expectedAbsolute
						text: 	qsTr("Absolute")
						name: 	"expectedAbsolute"
					}

					DoubleField 
					{
						name: 			"expectedNumber"
						enabled: 		expectedAbsolute.checked
						defaultValue: 	0
						min: 			0
						decimals: 		2
						visible: 		expectedAbsolute.checked
						fieldWidth: 	60
						label: 			euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)
					}
				}

				RowLayout 
				{
					RadioButton 
					{
						id: 		expectedRelative
						text: 		qsTr("Relative")
						name: 		"expectedRelative"
						checked: 	true
					}

					PercentField 
					{
						name: 			"expectedPercentage"
						enabled: 		expectedRelative.checked
						decimals: 		3
						defaultValue: 	0
						visible: 		expectedRelative.checked
						fieldWidth: 	40
					}
				}
			}

			GroupBox 
			{
				title: qsTr("Explanatory Text")

				RowLayout 
				{
					CheckBox 
					{
						id: 		explanatoryText
						text:	 	qsTr("Enable")
						name: 		"explanatoryText"
						checked: 	true
					}

					HelpButton 
					{ 
						helpPage:			"Audit/explanatoryText"
						toolTip: 			"Show explanatory text at each step of the analysis"
					}
				}
			}

			RadioButtonGroup 
			{
				id: 		cr
				title: 		qsTr("Control Risk")
				name: 		"CR"

				RadioButton 
				{
					text: 		qsTr("High")
					name: 		"High"
					checked: 	true
				}

				RadioButton 
				{
					text: 		qsTr("Medium")
					name: 		"Medium"
				}

				RadioButton 
				{
					text: 		qsTr("Low")
					name: 		"Low"
				}
			}

			RadioButtonGroup 
			{
				id: 		planningModel
				title: 		qsTr("Planning Distribution")
				name: 		"planningModel"

				RadioButton 
				{
					id: 		poisson
					text: 		qsTr("Poisson")
					name: 		"Poisson"
					checked: 	true
				}

				RadioButton 
				{
					id: 		binomial
					text: 		qsTr("Binomial")
					name: 		"binomial"
				}

				RadioButton 
				{
					id: 		hypergeometric
					text: 		qsTr("Hypergeometric")
					name: 		"hypergeometric"
				}
			}

			RadioButtonGroup 
			{
				id: 		valuta
				title: 		qsTr("Currency")
				name: 		"valuta"
				visible:	materialityAbsolute.checked

				RadioButton 
				{
					id: 		euroValuta
					text: 		qsTr("Euro (€)")
					name: 		"euroValuta"
					checked: 	true
				}

				RadioButton 
				{
					id: 		dollarValuta
					text: 		qsTr("Dollar ($)")
					name: 		"dollarValuta"
					checked: 	false
				}

				RowLayout 
				{
					RadioButton 
					{
						id: 		otherValuta
						text: 		qsTr("Other")
						name: 		"otherValuta"
						checked: 	false
					}

					TextField 
					{
						id: 			otherValutaName
						name: 			"otherValutaName"
						fieldWidth: 	100
						enabled: 		otherValuta.checked
						visible: 		otherValuta.checked
					}
				}
			}
		}
	}

	Section 
	{
		title: qsTr("Plots")

		GridLayout 
		{
			columns: 2

			GroupBox 
			{
				title: qsTr("Plots")

				CheckBox 
				{
					text: qsTr("Decision analysis")
					name: "decisionPlot"
				}
			}
		}
	}

	Item 
	{
		height: 			downloadReportPlanning.height
		Layout.fillWidth: 	true

		Button 
		{
			id: 			downloadReportPlanning
			enabled: 		materialityRelative.checked ? (populationSize.value != 0 && materialityPercentage.value != 0) : (populationSize.value != 0 && materialityValue.value != 0 && populationValue.value != 0)
			anchors.right: 	parent.right
			anchors.bottom: parent.bottom
			text: 			qsTr("<b>Download Report</b>")
			onClicked: 
			{
				form.exportResults()
			}
		}
	}
}
