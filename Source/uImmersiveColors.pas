// **************************************************************************************************
//
// Copyright (c) 2016-2017 Mahdi Safsafi.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// **************************************************************************************************

unit uImmersiveColors;

interface

uses
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.UITypes,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics;

type

{$REGION 'ImmersiveColorType'}
  TImmersiveColorType = ( //
    ImmersiveApplicationBackground, //
    ImmersiveApplicationBackgroundDarkTheme, //
    ImmersiveApplicationBackgroundLightTheme, //
    ImmersiveApplicationText, //
    ImmersiveApplicationTextDarkTheme, //
    ImmersiveApplicationTextLightTheme, //
    ImmersiveBootBackground, //
    ImmersiveBootConfirmationButton, //
    ImmersiveBootConfirmationButtonBackgroundDisabled, //
    ImmersiveBootConfirmationButtonBackgroundHover, //
    ImmersiveBootConfirmationButtonBackgroundPressed, //
    ImmersiveBootConfirmationButtonBackgroundRest, //
    ImmersiveBootConfirmationButtonBorderDisabled, //
    ImmersiveBootConfirmationButtonBorderHover, //
    ImmersiveBootConfirmationButtonBorderPressed, //
    ImmersiveBootConfirmationButtonBorderRest, //
    ImmersiveBootConfirmationButtonTextDisabled, //
    ImmersiveBootConfirmationButtonTextHover, //
    ImmersiveBootConfirmationButtonTextPressed, //
    ImmersiveBootConfirmationButtonTextRest, //
    ImmersiveBootDefaultConfirmationButtonBackgroundDisabled, //
    ImmersiveBootDefaultConfirmationButtonBackgroundHover, //
    ImmersiveBootDefaultConfirmationButtonBackgroundPressed, //
    ImmersiveBootDefaultConfirmationButtonBackgroundRest, //
    ImmersiveBootDefaultConfirmationButtonBorderDisabled, //
    ImmersiveBootDefaultConfirmationButtonBorderHover, //
    ImmersiveBootDefaultConfirmationButtonBorderPressed, //
    ImmersiveBootDefaultConfirmationButtonBorderRest, //
    ImmersiveBootDefaultConfirmationButtonTextDisabled, //
    ImmersiveBootDefaultConfirmationButtonTextHover, //
    ImmersiveBootDefaultConfirmationButtonTextPressed, //
    ImmersiveBootDefaultConfirmationButtonTextRest, //
    ImmersiveBootEditBackground, //
    ImmersiveBootEditBackgroundActive, //
    ImmersiveBootEditBackgroundDisabled, //
    ImmersiveBootEditBackgroundHover, //
    ImmersiveBootEditBackgroundRest, //
    ImmersiveBootEditBorderDisabled, //
    ImmersiveBootErrorText, //
    ImmersiveBootMenuButtonFocusRect, //
    ImmersiveBootMenuButtonGlyphBackground, //
    ImmersiveBootMenuButtonMouseHover, //
    ImmersiveBootMenuButtonPressedHighlight, //
    ImmersiveBootMenuButtonPressedText, //
    ImmersiveBootPrimaryText, //
    ImmersiveBootProgressText, //
    ImmersiveBootSecondaryText, //
    ImmersiveBootTextLinkHover, //
    ImmersiveBootTextLinkPressed, //
    ImmersiveBootTextLinkRest, //
    ImmersiveBootTitleText, //
    ImmersiveControlAppDefaultDarkButtonBackgroundHover, //
    ImmersiveControlAppDefaultDarkButtonBackgroundRest, //
    ImmersiveControlAppDefaultDarkButtonBorderHover, //
    ImmersiveControlAppDefaultDarkButtonBorderRest, //
    ImmersiveControlAppLightToggleTrackFillEnabled, //
    ImmersiveControlBlackButtonDisabled, //
    ImmersiveControlContextMenuBackgroundHover, //
    ImmersiveControlContextMenuBackgroundPressed, //
    ImmersiveControlContextMenuBackgroundRest, //
    ImmersiveControlContextMenuSeparator, //
    ImmersiveControlContextMenuTextHover, //
    ImmersiveControlContextMenuTextPressed, //
    ImmersiveControlContextMenuTextRest, //
    ImmersiveControlDarkAppButtonBackgroundDisabled, //
    ImmersiveControlDarkAppButtonBackgroundHover, //
    ImmersiveControlDarkAppButtonBackgroundPressed, //
    ImmersiveControlDarkAppButtonBackgroundRest, //
    ImmersiveControlDarkAppButtonBorderDisabled, //
    ImmersiveControlDarkAppButtonBorderHover, //
    ImmersiveControlDarkAppButtonBorderPressed, //
    ImmersiveControlDarkAppButtonBorderRest, //
    ImmersiveControlDarkAppButtonTextDisabled, //
    ImmersiveControlDarkAppButtonTextHover, //
    ImmersiveControlDarkAppButtonTextPressed, //
    ImmersiveControlDarkAppButtonTextRest, //
    ImmersiveControlDarkButtonBackgroundDisabled, //
    ImmersiveControlDarkButtonBackgroundHover, //
    ImmersiveControlDarkButtonBackgroundPressed, //
    ImmersiveControlDarkButtonBackgroundRest, //
    ImmersiveControlDarkButtonBorderDisabled, //
    ImmersiveControlDarkButtonBorderHover, //
    ImmersiveControlDarkButtonBorderPressed, //
    ImmersiveControlDarkButtonBorderRest, //
    ImmersiveControlDarkButtonTextDisabled, //
    ImmersiveControlDarkButtonTextHover, //
    ImmersiveControlDarkButtonTextPressed, //
    ImmersiveControlDarkButtonTextRest, //
    ImmersiveControlDarkCheckboxBackgroundDisabled, //
    ImmersiveControlDarkCheckboxBackgroundHover, //
    ImmersiveControlDarkCheckboxBackgroundPressed, //
    ImmersiveControlDarkCheckboxBackgroundRest, //
    ImmersiveControlDarkCheckboxBorderDisabled, //
    ImmersiveControlDarkCheckboxBorderHover, //
    ImmersiveControlDarkCheckboxBorderPressed, //
    ImmersiveControlDarkCheckboxBorderRest, //
    ImmersiveControlDarkCheckboxGlyphDisabled, //
    ImmersiveControlDarkCheckboxGlyphHover, //
    ImmersiveControlDarkCheckboxGlyphPressed, //
    ImmersiveControlDarkCheckboxGlyphRest, //
    ImmersiveControlDarkCheckboxLabelDisabled, //
    ImmersiveControlDarkCheckboxLabelHover, //
    ImmersiveControlDarkCheckboxLabelPressed, //
    ImmersiveControlDarkCheckboxLabelRest, //
    ImmersiveControlDarkFocusRect, //
    ImmersiveControlDarkLinkDisabled, //
    ImmersiveControlDarkLinkHover, //
    ImmersiveControlDarkLinkPressed, //
    ImmersiveControlDarkLinkRest, //
    ImmersiveControlDarkLinkVisited, //
    ImmersiveControlDarkPanningIndicator, //
    ImmersiveControlDarkProgressBackground, //
    ImmersiveControlDarkProgressForeground, //
    ImmersiveControlDarkRichEditBackgroundDisabled, //
    ImmersiveControlDarkRichEditBackgroundFocus, //
    ImmersiveControlDarkRichEditBackgroundHover, //
    ImmersiveControlDarkRichEditBackgroundPressed, //
    ImmersiveControlDarkRichEditBackgroundRest, //
    ImmersiveControlDarkRichEditBorderDisabled, //
    ImmersiveControlDarkRichEditBorderFocus, //
    ImmersiveControlDarkRichEditBorderHover, //
    ImmersiveControlDarkRichEditBorderPressed, //
    ImmersiveControlDarkRichEditBorderRest, //
    ImmersiveControlDarkRichEditButtonBackgroundHover, //
    ImmersiveControlDarkRichEditButtonBackgroundPressed, //
    ImmersiveControlDarkRichEditButtonBackgroundRest, //
    ImmersiveControlDarkRichEditButtonGlyphHover, //
    ImmersiveControlDarkRichEditButtonGlyphPressed, //
    ImmersiveControlDarkRichEditButtonGlyphRest, //
    ImmersiveControlDarkRichEditHighlight, //
    ImmersiveControlDarkRichEditPromptTextDisabled, //
    ImmersiveControlDarkRichEditPromptTextFocus, //
    ImmersiveControlDarkRichEditPromptTextRest, //
    ImmersiveControlDarkRichEditStartPromptBackgroundHover, //
    ImmersiveControlDarkRichEditStartPromptBackgroundRest, //
    ImmersiveControlDarkRichEditStartPromptButtonGlyphHover, //
    ImmersiveControlDarkRichEditStartPromptTextRest, //
    ImmersiveControlDarkRichEditTextDisabled, //
    ImmersiveControlDarkRichEditTextFocus, //
    ImmersiveControlDarkRichEditTextHelper, //
    ImmersiveControlDarkRichEditTextHighlighted, //
    ImmersiveControlDarkRichEditTextHover, //
    ImmersiveControlDarkRichEditTextRest, //
    ImmersiveControlDarkRoundButtonFillLayerDisabled, //
    ImmersiveControlDarkRoundButtonFillLayerHover, //
    ImmersiveControlDarkRoundButtonFillLayerPressed, //
    ImmersiveControlDarkRoundButtonFillLayerRest, //
    ImmersiveControlDarkRoundButtonGlyphDisabled, //
    ImmersiveControlDarkRoundButtonGlyphLayerHover, //
    ImmersiveControlDarkRoundButtonGlyphLayerPressed, //
    ImmersiveControlDarkRoundButtonGlyphLayerRest, //
    ImmersiveControlDarkRoundButtonOutlineDisabled, //
    ImmersiveControlDarkRoundButtonOutlineLayerHover, //
    ImmersiveControlDarkRoundButtonOutlineLayerPressed, //
    ImmersiveControlDarkRoundButtonOutlineLayerRest, //
    ImmersiveControlDarkScrollbarButtonGlyphHover, //
    ImmersiveControlDarkScrollbarButtonGlyphPressed, //
    ImmersiveControlDarkScrollbarButtonGlyphRest, //
    ImmersiveControlDarkScrollbarButtonHover, //
    ImmersiveControlDarkScrollbarButtonPressed, //
    ImmersiveControlDarkScrollbarButtonRest, //
    ImmersiveControlDarkScrollbarThumbHover, //
    ImmersiveControlDarkScrollbarThumbPressed, //
    ImmersiveControlDarkScrollbarThumbRest, //
    ImmersiveControlDarkScrollbarTrack, //
    ImmersiveControlDarkSelectBackgroundDisabled, //
    ImmersiveControlDarkSelectBackgroundHover, //
    ImmersiveControlDarkSelectBackgroundPressed, //
    ImmersiveControlDarkSelectBackgroundRest, //
    ImmersiveControlDarkSelectBorderDisabled, //
    ImmersiveControlDarkSelectBorderHover, //
    ImmersiveControlDarkSelectBorderPressed, //
    ImmersiveControlDarkSelectBorderRest, //
    ImmersiveControlDarkSelectGlyphDisabled, //
    ImmersiveControlDarkSelectGlyphRest, //
    ImmersiveControlDarkSelectHighlightedSecondaryTextPressed, //
    ImmersiveControlDarkSelectHighlightedTextPressed, //
    ImmersiveControlDarkSelectHighlightHover, //
    ImmersiveControlDarkSelectHighlightPressed, //
    ImmersiveControlDarkSelectHighlightSelected, //
    ImmersiveControlDarkSelectHighlightSelectedHover, //
    ImmersiveControlDarkSelectPopupBackgroundDisabled, //
    ImmersiveControlDarkSelectPopupBackgroundHover, //
    ImmersiveControlDarkSelectPopupBackgroundPressed, //
    ImmersiveControlDarkSelectPopupBackgroundRest, //
    ImmersiveControlDarkSelectPopupBackgroundSelected, //
    ImmersiveControlDarkSelectPopupBorder, //
    ImmersiveControlDarkSelectPopupTextDisabled, //
    ImmersiveControlDarkSelectPopupTextHover, //
    ImmersiveControlDarkSelectPopupTextPressed, //
    ImmersiveControlDarkSelectPopupTextRest, //
    ImmersiveControlDarkSelectPopupTextSelected, //
    ImmersiveControlDarkSelectSecondaryTextHighlighted, //
    ImmersiveControlDarkSelectSecondaryTextHover, //
    ImmersiveControlDarkSelectSecondaryTextPressed, //
    ImmersiveControlDarkSelectTextDisabled, //
    ImmersiveControlDarkSelectTextHighlighted, //
    ImmersiveControlDarkSelectTextHover, //
    ImmersiveControlDarkSelectTextPressed, //
    ImmersiveControlDarkSelectTextRest, //
    ImmersiveControlDarkSliderBorder, //
    ImmersiveControlDarkSliderThumbBorder, //
    ImmersiveControlDarkSliderThumbBorderDisabled, //
    ImmersiveControlDarkSliderThumbBorderHover, //
    ImmersiveControlDarkSliderThumbBorderPressed, //
    ImmersiveControlDarkSliderThumbBorderRest, //
    ImmersiveControlDarkSliderThumbDisabled, //
    ImmersiveControlDarkSliderThumbHover, //
    ImmersiveControlDarkSliderThumbPressed, //
    ImmersiveControlDarkSliderThumbRest, //
    ImmersiveControlDarkSliderTickMark, //
    ImmersiveControlDarkSliderTrackBackgroundDisabled, //
    ImmersiveControlDarkSliderTrackBackgroundHover, //
    ImmersiveControlDarkSliderTrackBackgroundPressed, //
    ImmersiveControlDarkSliderTrackBackgroundRest, //
    ImmersiveControlDarkSliderTrackBufferingDisabled, //
    ImmersiveControlDarkSliderTrackBufferingHover, //
    ImmersiveControlDarkSliderTrackBufferingPressed, //
    ImmersiveControlDarkSliderTrackBufferingRest, //
    ImmersiveControlDarkSliderTrackFillDisabled, //
    ImmersiveControlDarkSliderTrackFillHover, //
    ImmersiveControlDarkSliderTrackFillPressed, //
    ImmersiveControlDarkSliderTrackFillRest, //
    ImmersiveControlDarkToggleLabelDisabled, //
    ImmersiveControlDarkToggleLabelEnabled, //
    ImmersiveControlDarkToggleOnOffTextDisabled, //
    ImmersiveControlDarkToggleOnOffTextEnabled, //
    ImmersiveControlDarkToggleThumbDisabled, //
    ImmersiveControlDarkToggleThumbEnabled, //
    ImmersiveControlDarkToggleTrackBackgroundDisabled, //
    ImmersiveControlDarkToggleTrackBackgroundEnabled, //
    ImmersiveControlDarkToggleTrackBackgroundHover, //
    ImmersiveControlDarkToggleTrackBackgroundPressed, //
    ImmersiveControlDarkToggleTrackBackgroundRest, //
    ImmersiveControlDarkToggleTrackBorderDisabled, //
    ImmersiveControlDarkToggleTrackBorderEnabled, //
    ImmersiveControlDarkToggleTrackFillDisabled, //
    ImmersiveControlDarkToggleTrackFillEnabled, //
    ImmersiveControlDarkToggleTrackFillHover, //
    ImmersiveControlDarkToggleTrackFillPressed, //
    ImmersiveControlDarkToggleTrackFillRest, //
    ImmersiveControlDarkToggleTrackGutterDisabled, //
    ImmersiveControlDarkToggleTrackGutterEnabled, //
    ImmersiveControlDefaultDarkButtonBackgroundDisabled, //
    ImmersiveControlDefaultDarkButtonBackgroundHover, //
    ImmersiveControlDefaultDarkButtonBackgroundPressed, //
    ImmersiveControlDefaultDarkButtonBackgroundRest, //
    ImmersiveControlDefaultDarkButtonBorderDisabled, //
    ImmersiveControlDefaultDarkButtonBorderHover, //
    ImmersiveControlDefaultDarkButtonBorderPressed, //
    ImmersiveControlDefaultDarkButtonBorderRest, //
    ImmersiveControlDefaultDarkButtonTextDisabled, //
    ImmersiveControlDefaultDarkButtonTextHover, //
    ImmersiveControlDefaultDarkButtonTextPressed, //
    ImmersiveControlDefaultDarkButtonTextRest, //
    ImmersiveControlDefaultFocusRectDark, //
    ImmersiveControlDefaultFocusRectLight, //
    ImmersiveControlDefaultLightButtonBackgroundDisabled, //
    ImmersiveControlDefaultLightButtonBackgroundHover, //
    ImmersiveControlDefaultLightButtonBackgroundPressed, //
    ImmersiveControlDefaultLightButtonBackgroundRest, //
    ImmersiveControlDefaultLightButtonBorderDisabled, //
    ImmersiveControlDefaultLightButtonBorderHover, //
    ImmersiveControlDefaultLightButtonBorderPressed, //
    ImmersiveControlDefaultLightButtonBorderRest, //
    ImmersiveControlDefaultLightButtonTextDisabled, //
    ImmersiveControlDefaultLightButtonTextHover, //
    ImmersiveControlDefaultLightButtonTextPressed, //
    ImmersiveControlDefaultLightButtonTextRest, //
    ImmersiveControlFivePercentOpaqueWhite, //
    ImmersiveControlHighContrastBTNFACE, //
    ImmersiveControlHighContrastBTNTEXT, //
    ImmersiveControlHighContrastGRAYTEXT, //
    ImmersiveControlHighContrastHIGHLIGHT, //
    ImmersiveControlHighContrastHIGHLIGHTTEXT, //
    ImmersiveControlHighContrastHOTLIGHT, //
    ImmersiveControlHighContrastWINDOW, //
    ImmersiveControlHighContrastWINDOWTEXT, //
    ImmersiveControlLightAppButtonBackgroundDisabled, //
    ImmersiveControlLightAppButtonBackgroundHover, //
    ImmersiveControlLightAppButtonBackgroundPressed, //
    ImmersiveControlLightAppButtonBackgroundRest, //
    ImmersiveControlLightAppButtonBorderDisabled, //
    ImmersiveControlLightAppButtonBorderHover, //
    ImmersiveControlLightAppButtonBorderPressed, //
    ImmersiveControlLightAppButtonBorderRest, //
    ImmersiveControlLightAppButtonTextDisabled, //
    ImmersiveControlLightAppButtonTextHover, //
    ImmersiveControlLightAppButtonTextPressed, //
    ImmersiveControlLightAppButtonTextRest, //
    ImmersiveControlLightButtonBackgroundDisabled, //
    ImmersiveControlLightButtonBackgroundHover, //
    ImmersiveControlLightButtonBackgroundPressed, //
    ImmersiveControlLightButtonBackgroundRest, //
    ImmersiveControlLightButtonBorderDisabled, //
    ImmersiveControlLightButtonBorderHover, //
    ImmersiveControlLightButtonBorderPressed, //
    ImmersiveControlLightButtonBorderRest, //
    ImmersiveControlLightButtonTextDisabled, //
    ImmersiveControlLightButtonTextHover, //
    ImmersiveControlLightButtonTextPressed, //
    ImmersiveControlLightButtonTextRest, //
    ImmersiveControlLightCheckboxBackgroundDisabled, //
    ImmersiveControlLightCheckboxBackgroundHover, //
    ImmersiveControlLightCheckboxBackgroundPressed, //
    ImmersiveControlLightCheckboxBackgroundRest, //
    ImmersiveControlLightCheckboxBorderDisabled, //
    ImmersiveControlLightCheckboxBorderHover, //
    ImmersiveControlLightCheckboxBorderPressed, //
    ImmersiveControlLightCheckboxBorderRest, //
    ImmersiveControlLightCheckboxGlyphDisabled, //
    ImmersiveControlLightCheckboxGlyphHover, //
    ImmersiveControlLightCheckboxGlyphPressed, //
    ImmersiveControlLightCheckboxGlyphRest, //
    ImmersiveControlLightCheckboxLabelDisabled, //
    ImmersiveControlLightCheckboxLabelHover, //
    ImmersiveControlLightCheckboxLabelPressed, //
    ImmersiveControlLightCheckboxLabelRest, //
    ImmersiveControlLightFocusRect, //
    ImmersiveControlLightLinkDisabled, //
    ImmersiveControlLightLinkHover, //
    ImmersiveControlLightLinkPressed, //
    ImmersiveControlLightLinkRest, //
    ImmersiveControlLightLinkVisited, //
    ImmersiveControlLightPanningIndicator, //
    ImmersiveControlLightProgressBackground, //
    ImmersiveControlLightProgressForeground, //
    ImmersiveControlLightRadioButtonLabelDisabled, //
    ImmersiveControlLightRadioButtonLabelRest, //
    ImmersiveControlLightRadioButtonSelectedBackgroundDisabled, //
    ImmersiveControlLightRadioButtonSelectedBackgroundDown, //
    ImmersiveControlLightRadioButtonSelectedBackgroundHover, //
    ImmersiveControlLightRadioButtonSelectedBackgroundRest, //
    ImmersiveControlLightRadioButtonSelectedBackgroundUp, //
    ImmersiveControlLightRadioButtonSelectedBorderDisabled, //
    ImmersiveControlLightRadioButtonSelectedBorderDown, //
    ImmersiveControlLightRadioButtonSelectedBorderHover, //
    ImmersiveControlLightRadioButtonSelectedBorderRest, //
    ImmersiveControlLightRadioButtonSelectedBorderUp, //
    ImmersiveControlLightRadioButtonSelectedGlyphDisabled, //
    ImmersiveControlLightRadioButtonSelectedGlyphDown, //
    ImmersiveControlLightRadioButtonSelectedGlyphHover, //
    ImmersiveControlLightRadioButtonSelectedGlyphRest, //
    ImmersiveControlLightRadioButtonSelectedGlyphUp, //
    ImmersiveControlLightRadioButtonUnselectedBackgroundDisabled, //
    ImmersiveControlLightRadioButtonUnselectedBackgroundDown, //
    ImmersiveControlLightRadioButtonUnselectedBackgroundHover, //
    ImmersiveControlLightRadioButtonUnselectedBackgroundRest, //
    ImmersiveControlLightRadioButtonUnselectedBackgroundUp, //
    ImmersiveControlLightRadioButtonUnselectedBorderDisabled, //
    ImmersiveControlLightRadioButtonUnselectedBorderDown, //
    ImmersiveControlLightRadioButtonUnselectedBorderHover, //
    ImmersiveControlLightRadioButtonUnselectedBorderRest, //
    ImmersiveControlLightRadioButtonUnselectedBorderUp, //
    ImmersiveControlLightRadioButtonUnselectedGlyphDisabled, //
    ImmersiveControlLightRadioButtonUnselectedGlyphDown, //
    ImmersiveControlLightRadioButtonUnselectedGlyphHover, //
    ImmersiveControlLightRadioButtonUnselectedGlyphRest, //
    ImmersiveControlLightRadioButtonUnselectedGlyphUp, //
    ImmersiveControlLightRichEditBackgroundDisabled, //
    ImmersiveControlLightRichEditBackgroundFocus, //
    ImmersiveControlLightRichEditBackgroundHover, //
    ImmersiveControlLightRichEditBackgroundPressed, //
    ImmersiveControlLightRichEditBackgroundRest, //
    ImmersiveControlLightRichEditBorderDisabled, //
    ImmersiveControlLightRichEditBorderFocus, //
    ImmersiveControlLightRichEditBorderHover, //
    ImmersiveControlLightRichEditBorderPressed, //
    ImmersiveControlLightRichEditBorderRest, //
    ImmersiveControlLightRichEditButtonBackgroundHover, //
    ImmersiveControlLightRichEditButtonBackgroundPressed, //
    ImmersiveControlLightRichEditButtonBackgroundRest, //
    ImmersiveControlLightRichEditButtonGlyphHover, //
    ImmersiveControlLightRichEditButtonGlyphPressed, //
    ImmersiveControlLightRichEditButtonGlyphRest, //
    ImmersiveControlLightRichEditHighlight, //
    ImmersiveControlLightRichEditPromptTextDisabled, //
    ImmersiveControlLightRichEditPromptTextFocus, //
    ImmersiveControlLightRichEditPromptTextRest, //
    ImmersiveControlLightRichEditTextDisabled, //
    ImmersiveControlLightRichEditTextFocus, //
    ImmersiveControlLightRichEditTextHelper, //
    ImmersiveControlLightRichEditTextHighlighted, //
    ImmersiveControlLightRichEditTextHover, //
    ImmersiveControlLightRichEditTextRest, //
    ImmersiveControlLightRoundButtonFillLayerDisabled, //
    ImmersiveControlLightRoundButtonFillLayerHover, //
    ImmersiveControlLightRoundButtonFillLayerPressed, //
    ImmersiveControlLightRoundButtonFillLayerRest, //
    ImmersiveControlLightRoundButtonGlyphDisabled, //
    ImmersiveControlLightRoundButtonGlyphLayerHover, //
    ImmersiveControlLightRoundButtonGlyphLayerPressed, //
    ImmersiveControlLightRoundButtonGlyphLayerRest, //
    ImmersiveControlLightRoundButtonOutlineDisabled, //
    ImmersiveControlLightRoundButtonOutlineLayerHover, //
    ImmersiveControlLightRoundButtonOutlineLayerPressed, //
    ImmersiveControlLightRoundButtonOutlineLayerRest, //
    ImmersiveControlLightScrollbarButtonGlyphHover, //
    ImmersiveControlLightScrollbarButtonGlyphPressed, //
    ImmersiveControlLightScrollbarButtonGlyphRest, //
    ImmersiveControlLightScrollbarButtonHover, //
    ImmersiveControlLightScrollbarButtonPressed, //
    ImmersiveControlLightScrollbarButtonRest, //
    ImmersiveControlLightScrollbarThumbHover, //
    ImmersiveControlLightScrollbarThumbPressed, //
    ImmersiveControlLightScrollbarThumbRest, //
    ImmersiveControlLightScrollbarTrack, //
    ImmersiveControlLightSelectBackgroundDisabled, //
    ImmersiveControlLightSelectBackgroundHover, //
    ImmersiveControlLightSelectBackgroundPressed, //
    ImmersiveControlLightSelectBackgroundRest, //
    ImmersiveControlLightSelectBorderDisabled, //
    ImmersiveControlLightSelectBorderHover, //
    ImmersiveControlLightSelectBorderPressed, //
    ImmersiveControlLightSelectBorderRest, //
    ImmersiveControlLightSelectGlyphDisabled, //
    ImmersiveControlLightSelectGlyphRest, //
    ImmersiveControlLightSelectHighlightedTextPressed, //
    ImmersiveControlLightSelectHighlightHover, //
    ImmersiveControlLightSelectHighlightPressed, //
    ImmersiveControlLightSelectHighlightSelected, //
    ImmersiveControlLightSelectHighlightSelectedHover, //
    ImmersiveControlLightSelectPopupBackgroundDisabled, //
    ImmersiveControlLightSelectPopupBackgroundHover, //
    ImmersiveControlLightSelectPopupBackgroundPressed, //
    ImmersiveControlLightSelectPopupBackgroundRest, //
    ImmersiveControlLightSelectPopupBackgroundSelected, //
    ImmersiveControlLightSelectPopupBorder, //
    ImmersiveControlLightSelectPopupTextDisabled, //
    ImmersiveControlLightSelectPopupTextHover, //
    ImmersiveControlLightSelectPopupTextPressed, //
    ImmersiveControlLightSelectPopupTextRest, //
    ImmersiveControlLightSelectPopupTextSelected, //
    ImmersiveControlLightSelectTextDisabled, //
    ImmersiveControlLightSelectTextHighlighted, //
    ImmersiveControlLightSelectTextHover, //
    ImmersiveControlLightSelectTextPressed, //
    ImmersiveControlLightSelectTextRest, //
    ImmersiveControlLightSliderBorder, //
    ImmersiveControlLightSliderThumbBorder, //
    ImmersiveControlLightSliderThumbBorderDisabled, //
    ImmersiveControlLightSliderThumbBorderHover, //
    ImmersiveControlLightSliderThumbBorderPressed, //
    ImmersiveControlLightSliderThumbBorderRest, //
    ImmersiveControlLightSliderThumbDisabled, //
    ImmersiveControlLightSliderThumbHover, //
    ImmersiveControlLightSliderThumbPressed, //
    ImmersiveControlLightSliderThumbRest, //
    ImmersiveControlLightSliderTickMark, //
    ImmersiveControlLightSliderTrackBackgroundDisabled, //
    ImmersiveControlLightSliderTrackBackgroundHover, //
    ImmersiveControlLightSliderTrackBackgroundPressed, //
    ImmersiveControlLightSliderTrackBackgroundRest, //
    ImmersiveControlLightSliderTrackBufferingDisabled, //
    ImmersiveControlLightSliderTrackBufferingHover, //
    ImmersiveControlLightSliderTrackBufferingPressed, //
    ImmersiveControlLightSliderTrackBufferingRest, //
    ImmersiveControlLightSliderTrackFillDisabled, //
    ImmersiveControlLightSliderTrackFillHover, //
    ImmersiveControlLightSliderTrackFillPressed, //
    ImmersiveControlLightSliderTrackFillRest, //
    ImmersiveControlLightToggleLabelDisabled, //
    ImmersiveControlLightToggleLabelEnabled, //
    ImmersiveControlLightToggleOnOffTextDisabled, //
    ImmersiveControlLightToggleOnOffTextEnabled, //
    ImmersiveControlLightToggleThumbDisabled, //
    ImmersiveControlLightToggleThumbEnabled, //
    ImmersiveControlLightToggleTrackBackgroundDisabled, //
    ImmersiveControlLightToggleTrackBackgroundEnabled, //
    ImmersiveControlLightToggleTrackBackgroundHover, //
    ImmersiveControlLightToggleTrackBackgroundPressed, //
    ImmersiveControlLightToggleTrackBackgroundRest, //
    ImmersiveControlLightToggleTrackBorderDisabled, //
    ImmersiveControlLightToggleTrackBorderEnabled, //
    ImmersiveControlLightToggleTrackFillDisabled, //
    ImmersiveControlLightToggleTrackFillEnabled, //
    ImmersiveControlLightToggleTrackFillHover, //
    ImmersiveControlLightToggleTrackFillPressed, //
    ImmersiveControlLightToggleTrackFillRest, //
    ImmersiveControlLightToggleTrackGutterDisabled, //
    ImmersiveControlLightToggleTrackGutterEnabled, //
    ImmersiveControlProgressBorder, //
    ImmersiveControlRadioButtonBackgroundDisabledHover, //
    ImmersiveControlRadioButtonBackgroundDisabledPressed, //
    ImmersiveControlRadioButtonBackgroundDisabledSelected, //
    ImmersiveControlRadioButtonBackgroundSelected, //
    ImmersiveControlRadioButtonBorder, //
    ImmersiveControlRadioButtonSeparator, //
    ImmersiveControlRadioButtonTextDisabledHover, //
    ImmersiveControlRadioButtonTextDisabledPressed, //
    ImmersiveControlRadioButtonTextDisabledSelected, //
    ImmersiveControlRadioButtonTextSelected, //
    ImmersiveControlScrollbarBackground, //
    ImmersiveControlScrollbarButtonBackgroundHover, //
    ImmersiveControlScrollbarButtonBackgroundPressed, //
    ImmersiveControlScrollbarButtonBackgroundRest, //
    ImmersiveControlScrollbarButtonBorderHover, //
    ImmersiveControlScrollbarButtonBorderPressed, //
    ImmersiveControlScrollbarButtonBorderRest, //
    ImmersiveControlScrollbarButtonForegroundHover, //
    ImmersiveControlScrollbarButtonForegroundPressed, //
    ImmersiveControlScrollbarButtonForegroundRest, //
    ImmersiveControlScrollbarPanningIndicatorBackground, //
    ImmersiveControlScrollbarPanningIndicatorBorder, //
    ImmersiveControlScrollbarThumbBackgroundHover, //
    ImmersiveControlScrollbarThumbBackgroundPressed, //
    ImmersiveControlScrollbarThumbBackgroundRest, //
    ImmersiveControlScrollbarThumbBorderHover, //
    ImmersiveControlScrollbarThumbBorderPressed, //
    ImmersiveControlScrollbarThumbBorderRest, //
    ImmersiveControlScrollbarTrackBorder, //
    ImmersiveControlSkydriveCommerceBackground, //
    ImmersiveControlSkydriveCommerceDefaultButtonBackground, //
    ImmersiveControlSliderTooltipText, //
    ImmersiveControlSystemTileBorder, //
    ImmersiveControlTooltipBackground, //
    ImmersiveControlTooltipBorder, //
    ImmersiveControlTooltipDomainText, //
    ImmersiveControlTooltipText, //
    ImmersiveControlTransparent, //
    ImmersiveDarkAltHigh, //
    ImmersiveDarkAltLow, //
    ImmersiveDarkAltMedium, //
    ImmersiveDarkAltMediumHigh, //
    ImmersiveDarkAltMediumLow, //
    ImmersiveDarkBaseHigh, //
    ImmersiveDarkBaseLow, //
    ImmersiveDarkBaseMedium, //
    ImmersiveDarkBaseMediumHigh, //
    ImmersiveDarkBaseMediumLow, //
    ImmersiveDarkChromeAltLow, //
    ImmersiveDarkChromeBlackHigh, //
    ImmersiveDarkChromeBlackLow, //
    ImmersiveDarkChromeBlackMedium, //
    ImmersiveDarkChromeBlackMediumLow, //
    ImmersiveDarkChromeDisabledHigh, //
    ImmersiveDarkChromeDisabledLow, //
    ImmersiveDarkChromeHigh, //
    ImmersiveDarkChromeLow, //
    ImmersiveDarkChromeMedium, //
    ImmersiveDarkChromeMediumLow, //
    ImmersiveDarkChromeTaskbarBase, //
    ImmersiveDarkChromeWhite, //
    ImmersiveDarkListAccentHigh, //
    ImmersiveDarkListAccentLow, //
    ImmersiveDarkListAccentMedium, //
    ImmersiveDarkListLow, //
    ImmersiveDarkListMedium, //
    ImmersiveFilesAppAppBarBackground, //
    ImmersiveFilesAppAppBarForeground, //
    ImmersiveFilesAppAppBarHighContrastBorder, //
    ImmersiveFilesAppBackground, //
    ImmersiveFilesAppCommandRowDisabled, //
    ImmersiveFilesAppCommandRowHighlight, //
    ImmersiveFilesAppCommandRowHover, //
    ImmersiveFilesAppCommandRowPressed, //
    ImmersiveFilesAppCommandRowRest, //
    ImmersiveFilesAppDarkLinkText, //
    ImmersiveFilesAppDarkLinkTextDisabled, //
    ImmersiveFilesAppDarkLinkTextHover, //
    ImmersiveFilesAppDarkLinkTextPressed, //
    ImmersiveFilesAppDefaultButtonBackgroundHover, //
    ImmersiveFilesAppDefaultButtonBackgroundPressed, //
    ImmersiveFilesAppDefaultButtonBackgroundRest, //
    ImmersiveFilesAppDefaultButtonBorderHover, //
    ImmersiveFilesAppDefaultButtonBorderPressed, //
    ImmersiveFilesAppDefaultButtonBorderRest, //
    ImmersiveFilesAppDefaultButtonTextHover, //
    ImmersiveFilesAppDefaultButtonTextPressed, //
    ImmersiveFilesAppDefaultButtonTextRest, //
    ImmersiveFilesAppFolderHighContrastBorder, //
    ImmersiveFilesAppFolderHoverPrimaryText, //
    ImmersiveFilesAppFolderPrimaryText, //
    ImmersiveFilesAppFolderSelectionHoverPrimaryText, //
    ImmersiveFilesAppFolderSelectionPrimaryText, //
    ImmersiveFilesAppHoverBackground, //
    ImmersiveFilesAppItemBackground, //
    ImmersiveFilesAppItemPrimaryText, //
    ImmersiveFilesAppItemSecondaryText, //
    ImmersiveFilesAppLightErrorText, //
    ImmersiveFilesAppLightLinkText, //
    ImmersiveFilesAppLightLinkTextDisabled, //
    ImmersiveFilesAppLightLinkTextHover, //
    ImmersiveFilesAppLightLinkTextPressed, //
    ImmersiveFilesAppLocalFolderBackground, //
    ImmersiveFilesAppLocalFolderErrorText, //
    ImmersiveFilesAppLocalFolderHoverSecondaryText, //
    ImmersiveFilesAppLocalFolderSecondaryText, //
    ImmersiveFilesAppLocalFolderSelectionHoverSecondaryText, //
    ImmersiveFilesAppLocalFolderSelectionSecondaryText, //
    ImmersiveFilesAppPhotosAppSelectionBackground, //
    ImmersiveFilesAppPickerBackground, //
    ImmersiveFilesAppPickerButtonBackgroundDisabled, //
    ImmersiveFilesAppPickerButtonBorderDisabled, //
    ImmersiveFilesAppPickerButtonTextDisabled, //
    ImmersiveFilesAppPickerDefaultButtonBackgroundHover, //
    ImmersiveFilesAppPickerDefaultButtonBackgroundPressed, //
    ImmersiveFilesAppPickerDefaultButtonBackgroundRest, //
    ImmersiveFilesAppPickerDefaultButtonBorderHover, //
    ImmersiveFilesAppPickerDefaultButtonBorderPressed, //
    ImmersiveFilesAppPickerDefaultButtonBorderRest, //
    ImmersiveFilesAppPickerDefaultButtonTextHover, //
    ImmersiveFilesAppPickerDefaultButtonTextPressed, //
    ImmersiveFilesAppPickerDefaultButtonTextRest, //
    ImmersiveFilesAppPickerDocumentThumbnailBackground, //
    ImmersiveFilesAppPickerErrorText, //
    ImmersiveFilesAppPickerHoverBackground, //
    ImmersiveFilesAppPickerHoverPrimaryText, //
    ImmersiveFilesAppPickerHoverSecondaryText, //
    ImmersiveFilesAppPickerLocalFolderHoverBackground, //
    ImmersiveFilesAppPickerPaneBackground, //
    ImmersiveFilesAppPickerPaneHighlight, //
    ImmersiveFilesAppPickerPaneHoverBackground, //
    ImmersiveFilesAppPickerPaneHoverText, //
    ImmersiveFilesAppPickerPanePrimaryText, //
    ImmersiveFilesAppPickerPaneSecondaryText, //
    ImmersiveFilesAppPickerPrimaryText, //
    ImmersiveFilesAppPickerSecondaryText, //
    ImmersiveFilesAppPickerSelectionHoverBackground, //
    ImmersiveFilesAppPickerSelectionHoverPrimaryText, //
    ImmersiveFilesAppPickerSelectionHoverSecondaryText, //
    ImmersiveFilesAppPickerSelectionPrimaryText, //
    ImmersiveFilesAppPickerSelectionSecondaryText, //
    ImmersiveFilesAppPickerSkyDriveFolderHoverBackground, //
    ImmersiveFilesAppPickerThumbnailPlaceholder, //
    ImmersiveFilesAppPickerUnavailableText, //
    ImmersiveFilesAppProgressTrackBackground, //
    ImmersiveFilesAppProgressTrackFill, //
    ImmersiveFilesAppSearchBorder, //
    ImmersiveFilesAppSearchButtonBackground, //
    ImmersiveFilesAppSearchButtonForeground, //
    ImmersiveFilesAppSearchPrimaryText, //
    ImmersiveFilesAppSearchSecondaryText, //
    ImmersiveFilesAppSelectionBackground, //
    ImmersiveFilesAppSelectionForeground, //
    ImmersiveFilesAppSkyDriveEmphasisText, //
    ImmersiveFilesAppSkyDriveFolderBackground, //
    ImmersiveFilesAppSkyDriveFolderErrorText, //
    ImmersiveFilesAppSkyDriveFolderHoverSecondaryText, //
    ImmersiveFilesAppSkyDriveFolderSecondaryText, //
    ImmersiveFilesAppSkyDriveFolderSelectionHoverSecondaryText, //
    ImmersiveFilesAppSkyDriveFolderSelectionSecondaryText, //
    ImmersiveFilesAppSkyDriveUnavailableText, //
    ImmersiveFilesAppTabTextDisabled, //
    ImmersiveFilesAppTabTextHighlight, //
    ImmersiveFilesAppTabTextHover, //
    ImmersiveFilesAppTabTextPressed, //
    ImmersiveFilesAppTabTextRest, //
    ImmersiveFilesAppThumbnailHighContrastBorder, //
    ImmersiveFilesAppTouchSelectHighlightSelected, //
    ImmersiveFilesAppTouchSelectHighlightSelectedHover, //
    ImmersiveFilesAppTouchSelectHighlightSelectedHoverText, //
    ImmersiveFilesAppTouchSelectHighlightSelectedText, //
    ImmersiveHardwareAppBarBackground, //
    ImmersiveHardwareAppSwitcherBackground, //
    ImmersiveHardwareAppSwitcherHotTrack, //
    ImmersiveHardwareAppSwitcherTextOverlayBackground, //
    ImmersiveHardwareAppSwitcherThumbnailBorder, //
    ImmersiveHardwareAppSwitcherThumbnailBorderHover, //
    ImmersiveHardwareButtonDisabled, //
    ImmersiveHardwareButtonGlyphHover, //
    ImmersiveHardwareButtonGlyphPressed, //
    ImmersiveHardwareButtonHover, //
    ImmersiveHardwareButtonPressed, //
    ImmersiveHardwareButtonRest, //
    ImmersiveHardwareCharmsBarBackground, //
    ImmersiveHardwareCharmsBarBackgroundHotTrack, //
    ImmersiveHardwareCharmsBarBackgroundPressed, //
    ImmersiveHardwareCharmsBarBackgroundRest, //
    ImmersiveHardwareCharmsBarFlag, //
    ImmersiveHardwareCharmsBarLogoWash, //
    ImmersiveHardwareCharmsBarSelectedText, //
    ImmersiveHardwareCharmsBarText, //
    ImmersiveHardwareCharmsBarTextDisabled, //
    ImmersiveHardwareClockBackground, //
    ImmersiveHardwareClockText, //
    ImmersiveHardwareControlLink, //
    ImmersiveHardwareControlLinkDisabled, //
    ImmersiveHardwareControlLinkMouseHover, //
    ImmersiveHardwareControlLinkPressed, //
    ImmersiveHardwareControlLinkVisited, //
    ImmersiveHardwareCountdownText, //
    ImmersiveHardwareCropControlFill, //
    ImmersiveHardwareCropControlFillHover, //
    ImmersiveHardwareCropControlFillPressed, //
    ImmersiveHardwareCropControlOutline, //
    ImmersiveHardwareCropControlOutlineHover, //
    ImmersiveHardwareCropControlOutlinePressed, //
    ImmersiveHardwareDarkCandidateControlBackgroundHightlighted, //
    ImmersiveHardwareDarkCandidateControlBackgroundHover, //
    ImmersiveHardwareDarkCandidateControlBackgroundPressed, //
    ImmersiveHardwareDarkCandidateControlBackgroundRest, //
    ImmersiveHardwareDarkCandidateControlHighlightedSecondaryTextPressed, //
    ImmersiveHardwareDarkCandidateControlHighlightSelected, //
    ImmersiveHardwareDarkCandidateControlSecondaryTextHighlighted, //
    ImmersiveHardwareDarkCandidateControlSecondaryTextHover, //
    ImmersiveHardwareDarkCandidateControlSecondaryTextPressed, //
    ImmersiveHardwareDarkCandidateControlSecondaryTextRest, //
    ImmersiveHardwareDarkCandidateControlSecondaryTextSelect, //
    ImmersiveHardwareDarkCandidateControlTextHover, //
    ImmersiveHardwareDarkCandidateControlTextPressed, //
    ImmersiveHardwareDarkCandidateControlTextRest, //
    ImmersiveHardwareDarkCandidateControlTextSelect, //
    ImmersiveHardwareDarkKeyboardBackground, //
    ImmersiveHardwareDarkKeyboardChildKeyKeyText, //
    ImmersiveHardwareDarkKeyboardChildPanelBackground, //
    ImmersiveHardwareDarkKeyboardChildPanelKeyBackground, //
    ImmersiveHardwareDarkKeyboardEmojiCategoryTextSelected, //
    ImmersiveHardwareDarkKeyboardFunctionKeyBackground, //
    ImmersiveHardwareDarkKeyboardFunctionKeyText, //
    ImmersiveHardwareDarkKeyboardFunctionKeyTextDisabled, //
    ImmersiveHardwareDarkKeyboardKeyBackgroundHover, //
    ImmersiveHardwareDarkKeyboardKeyBackgroundPressed, //
    ImmersiveHardwareDarkKeyboardKeyBackgroundRest, //
    ImmersiveHardwareDarkKeyboardKeyDefaultBackgroundRest, //
    ImmersiveHardwareDarkKeyboardKeyDefaultPrimaryTextRest, //
    ImmersiveHardwareDarkKeyboardKeyPrimaryTextDisabled, //
    ImmersiveHardwareDarkKeyboardKeyPrimaryTextHover, //
    ImmersiveHardwareDarkKeyboardKeyPrimaryTextPressed, //
    ImmersiveHardwareDarkKeyboardKeyPrimaryTextRest, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextChildKeyDisabled, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextChildKeyHover, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextChildKeyPressed, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextChildKeyRest, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextModifierKeyDisabled, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextModifierKeyHover, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextModifierKeyPressed, //
    ImmersiveHardwareDarkKeyboardKeySecondaryTextModifierKeyRest, //
    ImmersiveHardwareDarkKeyboardNumberKeyBackground, //
    ImmersiveHardwareDarkKeyboardNumberKeyText, //
    ImmersiveHardwareDarkKeyboardSpaceGripper, //
    ImmersiveHardwareDarkKeyboardThumbCentralNumberKeyBackground, //
    ImmersiveHardwareDarkKeyboardThumbCentralNumberKeyText, //
    ImmersiveHardwareDarkKeyboardThumbResizeGripperBackgroundRest, //
    ImmersiveHardwareDarkRoundButtonFillLayerDisabled, //
    ImmersiveHardwareDarkRoundButtonFillLayerHover, //
    ImmersiveHardwareDarkRoundButtonFillLayerPressed, //
    ImmersiveHardwareDarkRoundButtonFillLayerRest, //
    ImmersiveHardwareDarkRoundButtonGlyphDisabled, //
    ImmersiveHardwareDarkRoundButtonGlyphLayerHover, //
    ImmersiveHardwareDarkRoundButtonGlyphLayerPressed, //
    ImmersiveHardwareDarkRoundButtonGlyphLayerRest, //
    ImmersiveHardwareDarkRoundButtonOutlineDisabled, //
    ImmersiveHardwareDarkRoundButtonOutlineLayerHover, //
    ImmersiveHardwareDarkRoundButtonOutlineLayerPressed, //
    ImmersiveHardwareDarkRoundButtonOutlineLayerRest, //
    ImmersiveHardwareDefaultKeyboardKeyBackgroundHover, //
    ImmersiveHardwareDefaultKeyboardKeyBackgroundRest, //
    ImmersiveHardwareDefaultKeyboardKeyPrimaryTextRest, //
    ImmersiveHardwareDefaultKeyboardKeySecondaryTextRest, //
    ImmersiveHardwareFlipViewFillHover, //
    ImmersiveHardwareFlipViewFillPressed, //
    ImmersiveHardwareFlipViewFillRest, //
    ImmersiveHardwareFlipViewGlyphHover, //
    ImmersiveHardwareFlipViewGlyphPressed, //
    ImmersiveHardwareFlipViewGlyphRest, //
    ImmersiveHardwareFlipViewOutlineHover, //
    ImmersiveHardwareFlipViewOutlinePressed, //
    ImmersiveHardwareFlipViewOutlineRest, //
    ImmersiveHardwareGenericBackground, //
    ImmersiveHardwareGutterBackground, //
    ImmersiveHardwareGutterDown, //
    ImmersiveHardwareGutterIndicator, //
    ImmersiveHardwareGutterRest, //
    ImmersiveHardwareHandwritingPanelBorder, //
    ImmersiveHardwareHandwritingPanelButtonBorder, //
    ImmersiveHardwareHandwritingPanelButtonHover, //
    ImmersiveHardwareHandwritingPanelButtonPress, //
    ImmersiveHardwareHandwritingPanelButtonRest, //
    ImmersiveHardwareHandwritingPanelCharChevronPressed, //
    ImmersiveHardwareHandwritingPanelCharChevronRest, //
    ImmersiveHardwareHandwritingPanelConversionHoveredText, //
    ImmersiveHardwareHandwritingPanelConversionSelectedBackground, //
    ImmersiveHardwareHandwritingPanelConversionSelectedText, //
    ImmersiveHardwareHandwritingPanelConversionToggledText, //
    ImmersiveHardwareHandwritingPanelConversionUnselectedBackground, //
    ImmersiveHardwareHandwritingPanelConversionUnselectedText, //
    ImmersiveHardwareHandwritingPanelCorrectionText, //
    ImmersiveHardwareHandwritingPanelHoverWord, //
    ImmersiveHardwareHandwritingPanelInsertModeCharacter, //
    ImmersiveHardwareHandwritingPanelKanjiConversionBackground, //
    ImmersiveHardwareHandwritingPanelKanjiConversionBackgroundHovered, //
    ImmersiveHardwareHandwritingPanelKanjiConversionBorder, //
    ImmersiveHardwareHandwritingPanelKanjiConversionText, //
    ImmersiveHardwareHandwritingPanelMatchedText, //
    ImmersiveHardwareHandwritingPanelSuggestedWord, //
    ImmersiveHardwareHelpStickerAccent, //
    ImmersiveHardwareHelpStickerBackground, //
    ImmersiveHardwareHelpStickerBorder, //
    ImmersiveHardwareKeyboardBackground, //
    ImmersiveHardwareKeyboardChildKeyKeyText, //
    ImmersiveHardwareKeyboardChildPanelBackground, //
    ImmersiveHardwareKeyboardChildPanelKeyBackground, //
    ImmersiveHardwareKeyboardDarkSpaceKeyBackgroundPressed, //
    ImmersiveHardwareKeyboardEmojiCategoryTextSelected, //
    ImmersiveHardwareKeyboardFunctionKeyBackground, //
    ImmersiveHardwareKeyboardFunctionKeyBackgroundHover, //
    ImmersiveHardwareKeyboardFunctionKeyText, //
    ImmersiveHardwareKeyboardFunctionKeyTextDisabled, //
    ImmersiveHardwareKeyboardKeyBackgroundDisabled, //
    ImmersiveHardwareKeyboardKeyBackgroundHover, //
    ImmersiveHardwareKeyboardKeyBackgroundPressed, //
    ImmersiveHardwareKeyboardKeyBackgroundRest, //
    ImmersiveHardwareKeyboardKeyBorder, //
    ImmersiveHardwareKeyboardKeyDockCloseRest, //
    ImmersiveHardwareKeyboardKeyPrimaryTextDisabled, //
    ImmersiveHardwareKeyboardKeyPrimaryTextPressed, //
    ImmersiveHardwareKeyboardKeyPrimaryTextRest, //
    ImmersiveHardwareKeyboardKeySecondaryTextDisabled, //
    ImmersiveHardwareKeyboardKeySecondaryTextPressed, //
    ImmersiveHardwareKeyboardKeySecondaryTextRest, //
    ImmersiveHardwareKeyboardNumberKeyBackground, //
    ImmersiveHardwareKeyboardNumberKeyBackgroundHover, //
    ImmersiveHardwareKeyboardNumberKeyText, //
    ImmersiveHardwareKeyboardThumbCentralNumberKeyBackground, //
    ImmersiveHardwareKeyboardThumbCentralNumberKeyText, //
    ImmersiveHardwareKeyboardThumbResizeGripperBackgroundRest, //
    ImmersiveHardwareKeyboardTitleBarText, //
    ImmersiveHardwareKeyboardTitleBarTextDisabled, //
    ImmersiveHardwarePinFlyoutFlipViewFillHover, //
    ImmersiveHardwarePinFlyoutFlipViewFillPressed, //
    ImmersiveHardwarePinFlyoutFlipViewFillRest, //
    ImmersiveHardwarePinFlyoutFlipViewGlyphHover, //
    ImmersiveHardwarePinFlyoutFlipViewGlyphPressed, //
    ImmersiveHardwarePinFlyoutFlipViewGlyphRest, //
    ImmersiveHardwarePinFlyoutFlipViewOutlineHover, //
    ImmersiveHardwarePinFlyoutFlipViewOutlinePressed, //
    ImmersiveHardwarePinFlyoutFlipViewOutlineRest, //
    ImmersiveHardwarePlaybackBarProgressFill, //
    ImmersiveHardwarePlaybackBarRest, //
    ImmersiveHardwarePlaybackBarTrim, //
    ImmersiveHardwarePlaybackButtonFill, //
    ImmersiveHardwarePlaybackButtonFillHover, //
    ImmersiveHardwarePlaybackButtonFillPressed, //
    ImmersiveHardwarePlaybackButtonGlyph, //
    ImmersiveHardwarePlaybackButtonGlyphHover, //
    ImmersiveHardwarePlaybackButtonGlyphPressed, //
    ImmersiveHardwarePlaybackButtonOutline, //
    ImmersiveHardwarePrimaryText, //
    ImmersiveHardwareScreenFill, //
    ImmersiveHardwareScrubberControlFill, //
    ImmersiveHardwareScrubberControlFillHover, //
    ImmersiveHardwareScrubberControlFillPressed, //
    ImmersiveHardwareScrubberControlOutline, //
    ImmersiveHardwareScrubberControlOutlineHover, //
    ImmersiveHardwareScrubberControlOutlinePressed, //
    ImmersiveHardwareSemanticZoomBackground, //
    ImmersiveHardwareSemanticZoomBackgroundHover, //
    ImmersiveHardwareSemanticZoomBackgroundPressed, //
    ImmersiveHardwareSemanticZoomForeground, //
    ImmersiveHardwareSemanticZoomForegroundHover, //
    ImmersiveHardwareSemanticZoomForegroundPressed, //
    ImmersiveHardwareSettingCharmSystemPaneButtonDisabled, //
    ImmersiveHardwareSettingCharmSystemPaneButtonHover, //
    ImmersiveHardwareSettingCharmSystemPaneButtonPressed, //
    ImmersiveHardwareSettingCharmSystemPaneButtonRest, //
    ImmersiveHardwareSettingCharmSystemPaneButtonSelected, //
    ImmersiveHardwareSettingCharmSystemPaneButtonText, //
    ImmersiveHardwareSettingCharmSystemPaneButtonTextDisabled, //
    ImmersiveHardwareSettingCharmSystemPaneButtonTextHover, //
    ImmersiveHardwareSettingCharmSystemPaneButtonTextPressed, //
    ImmersiveHardwareSettingCharmSystemPaneButtonTextSelected, //
    ImmersiveHardwareTextPredictionBackgroundPressed, //
    ImmersiveHardwareTextPredictionBackgroundRest, //
    ImmersiveHardwareTextPredictionBorder, //
    ImmersiveHardwareTextPredictionTextPressed, //
    ImmersiveHardwareTextPredictionTextRest, //
    ImmersiveHardwareTitleBarBackground, //
    ImmersiveHardwareTitleBarCloseButtonHover, //
    ImmersiveHardwareTitleBarCloseButtonPressed, //
    ImmersiveHardwareTitleBarMinimizeButtonHover, //
    ImmersiveHardwareTitleBarMinimizeButtonPressed, //
    ImmersiveHardwareTrimBarProgressFill, //
    ImmersiveHardwareTrimControlFill, //
    ImmersiveHardwareTrimControlFillHover, //
    ImmersiveHardwareTrimControlFillPressed, //
    ImmersiveHardwareTrimControlOutline, //
    ImmersiveHardwareTrimControlOutlineHover, //
    ImmersiveHardwareTrimControlOutlinePressed, //
    ImmersiveHardwareWin8Pillarbox, //
    ImmersiveInputSwitchColorDarkBackground, //
    ImmersiveInputSwitchColorDarkButtonBackgroundHover, //
    ImmersiveInputSwitchColorDarkButtonBackgroundPressed, //
    ImmersiveInputSwitchColorDarkButtonSecondaryTextHover, //
    ImmersiveInputSwitchColorDarkButtonSecondaryTextPressed, //
    ImmersiveInputSwitchColorDarkButtonSecondaryTextRest, //
    ImmersiveInputSwitchColorDarkButtonTextHover, //
    ImmersiveInputSwitchColorDarkButtonTextPressed, //
    ImmersiveInputSwitchColorDarkButtonTextRest, //
    ImmersiveInputSwitchColorDarkDisabledText, //
    ImmersiveInputSwitchColorDarkRadioButtonBackgroundDisabledSelected, //
    ImmersiveInputSwitchColorDarkRadioButtonBackgroundSelected, //
    ImmersiveInputSwitchColorDarkRadioButtonBorder, //
    ImmersiveInputSwitchColorDarkRadioButtonTextDisabledSelected, //
    ImmersiveInputSwitchColorDarkSelectionPrimaryText, //
    ImmersiveInputSwitchColorDarkSeparatorLine, //
    ImmersiveInputSwitchColorDarkWindowBorder, //
    ImmersiveInputSwitchDarkBackground, //
    ImmersiveInputSwitchDarkButtonBackgroundHover, //
    ImmersiveInputSwitchDarkButtonBackgroundPressed, //
    ImmersiveInputSwitchDarkButtonSecondaryTextHover, //
    ImmersiveInputSwitchDarkButtonSecondaryTextPressed, //
    ImmersiveInputSwitchDarkButtonSecondaryTextRest, //
    ImmersiveInputSwitchDarkButtonTextHover, //
    ImmersiveInputSwitchDarkButtonTextPressed, //
    ImmersiveInputSwitchDarkButtonTextRest, //
    ImmersiveInputSwitchDarkDisabledText, //
    ImmersiveInputSwitchDarkRadioButtonBackgroundDisabledSelected, //
    ImmersiveInputSwitchDarkRadioButtonBackgroundSelected, //
    ImmersiveInputSwitchDarkRadioButtonBorder, //
    ImmersiveInputSwitchDarkRadioButtonTextDisabledSelected, //
    ImmersiveInputSwitchDarkSelectionPrimaryText, //
    ImmersiveInputSwitchDarkSeparatorLine, //
    ImmersiveInputSwitchDarkWindowBorder, //
    ImmersiveInputSwitchLightBackground, //
    ImmersiveInputSwitchLightButtonBackgroundHover, //
    ImmersiveInputSwitchLightButtonBackgroundPressed, //
    ImmersiveInputSwitchLightButtonSecondaryTextHover, //
    ImmersiveInputSwitchLightButtonSecondaryTextPressed, //
    ImmersiveInputSwitchLightButtonSecondaryTextRest, //
    ImmersiveInputSwitchLightButtonTextHover, //
    ImmersiveInputSwitchLightButtonTextPressed, //
    ImmersiveInputSwitchLightButtonTextRest, //
    ImmersiveInputSwitchLightDisabledText, //
    ImmersiveInputSwitchLightRadioButtonBackgroundDisabledSelected, //
    ImmersiveInputSwitchLightRadioButtonBackgroundSelected, //
    ImmersiveInputSwitchLightRadioButtonBorder, //
    ImmersiveInputSwitchLightRadioButtonTextDisabledSelected, //
    ImmersiveInputSwitchLightSelectionPrimaryText, //
    ImmersiveInputSwitchLightSeparatorLine, //
    ImmersiveInputSwitchLightWindowBorder, //
    ImmersiveLightAltHigh, //
    ImmersiveLightAltLow, //
    ImmersiveLightAltMedium, //
    ImmersiveLightAltMediumHigh, //
    ImmersiveLightAltMediumLow, //
    ImmersiveLightBackground, //
    ImmersiveLightBackgroundDisabled, //
    ImmersiveLightBaseHigh, //
    ImmersiveLightBaseLow, //
    ImmersiveLightBaseMedium, //
    ImmersiveLightBaseMediumHigh, //
    ImmersiveLightBaseMediumLow, //
    ImmersiveLightBorder, //
    ImmersiveLightChromeAltLow, //
    ImmersiveLightChromeBlackHigh, //
    ImmersiveLightChromeBlackLow, //
    ImmersiveLightChromeBlackMedium, //
    ImmersiveLightChromeBlackMediumLow, //
    ImmersiveLightChromeDisabledHigh, //
    ImmersiveLightChromeDisabledLow, //
    ImmersiveLightChromeHigh, //
    ImmersiveLightChromeLow, //
    ImmersiveLightChromeMedium, //
    ImmersiveLightChromeMediumLow, //
    ImmersiveLightChromeTaskbarBase, //
    ImmersiveLightChromeWhite, //
    ImmersiveLightControlLink, //
    ImmersiveLightControlLinkBackgroundPressed, //
    ImmersiveLightControlLinkDisabled, //
    ImmersiveLightControlLinkForegroundPressed, //
    ImmersiveLightControlLinkMouseHover, //
    ImmersiveLightControlLinkPressed, //
    ImmersiveLightControlLinkVisited, //
    ImmersiveLightDesktopToastBackground, //
    ImmersiveLightDisabledText, //
    ImmersiveLightDivider, //
    ImmersiveLightEntityItemBackgroundHover, //
    ImmersiveLightEntityItemBackgroundSelected, //
    ImmersiveLightEntityItemBorderHover, //
    ImmersiveLightEntityItemBorderSelected, //
    ImmersiveLightFocusRect, //
    ImmersiveLightHighlight, //
    ImmersiveLightHoverBackground, //
    ImmersiveLightHoverBackgroundTransparent, //
    ImmersiveLightHoverPrimaryText, //
    ImmersiveLightHoverSecondaryText, //
    ImmersiveLightIconBorder, //
    ImmersiveLightInlineErrorText, //
    ImmersiveLightListAccentHigh, //
    ImmersiveLightListAccentLow, //
    ImmersiveLightListAccentMedium, //
    ImmersiveLightListLow, //
    ImmersiveLightListMedium, //
    ImmersiveLightNavBackground, //
    ImmersiveLightPCSettingsNavBarItemBackgroundHover, //
    ImmersiveLightPCSettingsNavBarItemBackgroundTouchPressed, //
    ImmersiveLightPCSettingsSearchButtonBackgroundHover, //
    ImmersiveLightPCSettingsSearchButtonBackgroundPressed, //
    ImmersiveLightPCSettingsSearchButtonBackgroundRest, //
    ImmersiveLightPlaceholderBackground, //
    ImmersiveLightPrimaryChartFill, //
    ImmersiveLightPrimaryText, //
    ImmersiveLightSearchAdBackground, //
    ImmersiveLightSearchContactHeroAccentDark, //
    ImmersiveLightSearchContactHeroAccentHitHighlight, //
    ImmersiveLightSearchContactHeroAccentLight, //
    ImmersiveLightSearchContactHeroAccentLink, //
    ImmersiveLightSearchFinanceGreen, //
    ImmersiveLightSearchFinanceRed, //
    ImmersiveLightSearchHeroBackground, //
    ImmersiveLightSearchHeroCollageAccentDark, //
    ImmersiveLightSearchHeroCollageAccentLight, //
    ImmersiveLightSearchHeroMapAccentDark, //
    ImmersiveLightSearchHeroMapAccentLight, //
    ImmersiveLightSearchHitHighlight, //
    ImmersiveLightSearchNewsBackground, //
    ImmersiveLightSearchNewsBackgroundBorder, //
    ImmersiveLightSearchSettingTilesBackground, //
    ImmersiveLightSearchStoreRatingEmpty, //
    ImmersiveLightSearchStoreRatingFull, //
    ImmersiveLightSearchVideoTextWhite, //
    ImmersiveLightSearchWeatherBlue, //
    ImmersiveLightSecondaryChartFill, //
    ImmersiveLightSecondaryText, //
    ImmersiveLightSecondaryTextTransparent, //
    ImmersiveLightSelectedTabText, //
    ImmersiveLightSelectionBackground, //
    ImmersiveLightSelectionHoverBackground, //
    ImmersiveLightSelectionHoverPrimaryText, //
    ImmersiveLightSelectionHoverSecondaryText, //
    ImmersiveLightSelectionPrimaryText, //
    ImmersiveLightSelectionSecondaryText, //
    ImmersiveLightTabText, //
    ImmersiveLightTitleText, //
    ImmersiveLightWUError, //
    ImmersiveLightWUNormal, //
    ImmersiveLightWUWarning, //
    ImmersiveMultitaskingMTVActiveVirtualDesktopHotTrackOuterBorder, //
    ImmersiveMultitaskingMTVActiveVirtualDesktopOuterBorder, //
    ImmersiveMultitaskingMTVCloseGlyphHotTrack, //
    ImmersiveMultitaskingMTVCloseGlyphPressed, //
    ImmersiveMultitaskingMTVCloseGlyphRest, //
    ImmersiveMultitaskingMTVCloseOutlineHotTrack, //
    ImmersiveMultitaskingMTVCloseOutlinePressed, //
    ImmersiveMultitaskingMTVCloseOutlineRest, //
    ImmersiveMultitaskingMTVClosePlateHotTrack, //
    ImmersiveMultitaskingMTVClosePlatePressed, //
    ImmersiveMultitaskingMTVClosePlateRest, //
    ImmersiveMultitaskingMTVDimmingLayer, //
    ImmersiveMultitaskingMTVInactiveVirtualDesktopDim, //
    ImmersiveMultitaskingMTVNewDesktopFocusOutline, //
    ImmersiveMultitaskingMTVNewDesktopOutlineHotTrack, //
    ImmersiveMultitaskingMTVNewDesktopOutlinePressed, //
    ImmersiveMultitaskingMTVNewDesktopOutlineRest, //
    ImmersiveMultitaskingMTVNewDesktopPlateHotTrack, //
    ImmersiveMultitaskingMTVNewDesktopPlatePressed, //
    ImmersiveMultitaskingMTVNewDesktopPlateRest, //
    ImmersiveMultitaskingMTVNewDesktopTextHotTrack, //
    ImmersiveMultitaskingMTVNewDesktopTextPressed, //
    ImmersiveMultitaskingMTVNewDesktopTextRest, //
    ImmersiveMultitaskingMTVRestVirtualDesktopInner, //
    ImmersiveMultitaskingMTVScrollButtonGlyphDisabled, //
    ImmersiveMultitaskingMTVScrollButtonGlyphHotTrack, //
    ImmersiveMultitaskingMTVScrollButtonGlyphPressed, //
    ImmersiveMultitaskingMTVScrollButtonGlyphRest, //
    ImmersiveMultitaskingMTVScrollButtonOutlineDisabled, //
    ImmersiveMultitaskingMTVScrollButtonOutlineHotTrack, //
    ImmersiveMultitaskingMTVScrollButtonOutlinePressed, //
    ImmersiveMultitaskingMTVScrollButtonOutlineRest, //
    ImmersiveMultitaskingMTVScrollButtonPlateDisabled, //
    ImmersiveMultitaskingMTVScrollButtonPlateHotTrack, //
    ImmersiveMultitaskingMTVScrollButtonPlatePressed, //
    ImmersiveMultitaskingMTVScrollButtonPlateRest, //
    ImmersiveMultitaskingMTVScrollViewerBackground, //
    ImmersiveMultitaskingMTVScrollViewerOutline, //
    ImmersiveMultitaskingMTVSwitchItemTitle, //
    ImmersiveMultitaskingMTVThumbnailFocusInnerBorder, //
    ImmersiveMultitaskingMTVThumbnailFocusOuterBorder, //
    ImmersiveMultitaskingMTVThumbnailHotTrackInnerBorder, //
    ImmersiveMultitaskingMTVThumbnailHotTrackOuterBorder, //
    ImmersiveMultitaskingMTVThumbnailInnerBorder, //
    ImmersiveMultitaskingMTVVirtualDesktopBarBackground, //
    ImmersiveMultitaskingMTVVirtualDesktopBarOutline, //
    ImmersiveMultitaskingMTVVirtualDesktopDragDrop, //
    ImmersiveMultitaskingMTVVirtualDesktopHotTrackOuterBorder, //
    ImmersiveMultitaskingMTVVirtualDesktopInnerBorder, //
    ImmersiveMultitaskingMTVVirtualDesktopTitle, //
    ImmersiveSaturatedAltTabBackground, //
    ImmersiveSaturatedAltTabHoverRect, //
    ImmersiveSaturatedAltTabPressedRect, //
    ImmersiveSaturatedBackButtonBar, //
    ImmersiveSaturatedBackground, //
    ImmersiveSaturatedBackgroundDisabled, //
    ImmersiveSaturatedCommandRowDisabled, //
    ImmersiveSaturatedCommandRowHighlight, //
    ImmersiveSaturatedCommandRowHover, //
    ImmersiveSaturatedCommandRowPressed, //
    ImmersiveSaturatedCommandRowRest, //
    ImmersiveSaturatedControlLink, //
    ImmersiveSaturatedControlLinkBackgroundPressed, //
    ImmersiveSaturatedControlLinkDisabled, //
    ImmersiveSaturatedControlLinkForegroundPressed, //
    ImmersiveSaturatedControlLinkMouseHover, //
    ImmersiveSaturatedControlLinkPressed, //
    ImmersiveSaturatedControlLinkVisited, //
    ImmersiveSaturatedDefaultDarkFocusRect, //
    ImmersiveSaturatedDefaultLightFocusRect, //
    ImmersiveSaturatedDesktopToastBackground, //
    ImmersiveSaturatedDisabledText, //
    ImmersiveSaturatedDivider, //
    ImmersiveSaturatedFocusRect, //
    ImmersiveSaturatedFocusRectDark, //
    ImmersiveSaturatedFolderBackground, //
    ImmersiveSaturatedHighlight, //
    ImmersiveSaturatedHoverBackground, //
    ImmersiveSaturatedHoverPrimaryText, //
    ImmersiveSaturatedHoverSecondaryText, //
    ImmersiveSaturatedInlineErrorText, //
    ImmersiveSaturatedPrimaryText, //
    ImmersiveSaturatedSearchHighlight, //
    ImmersiveSaturatedSearchSecondaryText, //
    ImmersiveSaturatedSecondaryText, //
    ImmersiveSaturatedSecondaryTextDisabledTransparent, //
    ImmersiveSaturatedSecondaryTextTransparent, //
    ImmersiveSaturatedSelectionBackground, //
    ImmersiveSaturatedSelectionHoverBackground, //
    ImmersiveSaturatedSelectionHoverPrimaryText, //
    ImmersiveSaturatedSelectionHoverSecondaryText, //
    ImmersiveSaturatedSelectionPrimaryText, //
    ImmersiveSaturatedSelectionSecondaryText, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonDisabled, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonHover, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonPressed, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonRest, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonSelected, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonText, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonTextDisabled, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonTextHover, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonTextPressed, //
    ImmersiveSaturatedSettingCharmSystemPaneButtonTextSelected, //
    ImmersiveSaturatedSettingsCharmLinkHover, //
    ImmersiveSaturatedSuggestionTilesBackground, //
    ImmersiveSaturatedSystemToastBackground, //
    ImmersiveSaturatedThumbnailPlaceholder, //
    ImmersiveSaturatedWebWizardIndex, //
    ImmersiveStartAllAppsDivider, //
    ImmersiveStartAPVBackground, //
    ImmersiveStartAPVHeaderText, //
    ImmersiveStartAPVNewAppText, //
    ImmersiveStartAPVSemanticZoomTileBackground, //
    ImmersiveStartBackground, //
    ImmersiveStartBackgroundDisabled, //
    ImmersiveStartCmdBarBackground, //
    ImmersiveStartCmdBarButtonFillLayerHover, //
    ImmersiveStartCmdBarButtonFillLayerPressed, //
    ImmersiveStartCmdBarButtonFillLayerRest, //
    ImmersiveStartCmdBarButtonGlyphLayerHover, //
    ImmersiveStartCmdBarButtonGlyphLayerPressed, //
    ImmersiveStartCmdBarButtonGlyphLayerRest, //
    ImmersiveStartCmdBarButtonOutlineLayerHover, //
    ImmersiveStartCmdBarButtonOutlineLayerPressed, //
    ImmersiveStartCmdBarButtonOutlineLayerRest, //
    ImmersiveStartCommandRowDisabled, //
    ImmersiveStartCommandRowHighlight, //
    ImmersiveStartCommandRowHover, //
    ImmersiveStartCommandRowPressed, //
    ImmersiveStartCommandRowRest, //
    ImmersiveStartControlLink, //
    ImmersiveStartControlLinkBackgroundPressed, //
    ImmersiveStartControlLinkDisabled, //
    ImmersiveStartControlLinkForegroundPressed, //
    ImmersiveStartControlLinkMouseHover, //
    ImmersiveStartControlLinkPressed, //
    ImmersiveStartControlLinkVisited, //
    ImmersiveStartDarkTileInstallProgressFill, //
    ImmersiveStartDarkTileInstallProgressTrack, //
    ImmersiveStartDefaultDarkFocusRect, //
    ImmersiveStartDefaultLightFocusRect, //
    ImmersiveStartDesktopAppBackground, //
    ImmersiveStartDesktopBackgroundTattooAllAppsDivider, //
    ImmersiveStartDesktopBackgroundTattooSecondaryText, //
    ImmersiveStartDesktopBackgroundTattooStartPromptBackgroundHover, //
    ImmersiveStartDesktopBackgroundTattooStartPromptBackgroundRest, //
    ImmersiveStartDesktopBackgroundTattooTileDim, //
    ImmersiveStartDesktopBackgroundTattooViewSwitchButtonHover, //
    ImmersiveStartDesktopBackgroundTattooViewSwitchButtonRest, //
    ImmersiveStartDesktopTilesBackground, //
    ImmersiveStartDesktopTilesText, //
    ImmersiveStartDisabledText, //
    ImmersiveStartFocusRect, //
    ImmersiveStartFolderBackground, //
    ImmersiveStartGroupNameHeader, //
    ImmersiveStartHighlight, //
    ImmersiveStartHoverBackground, //
    ImmersiveStartHoverPrimaryText, //
    ImmersiveStartHoverSecondaryText, //
    ImmersiveStartInlineErrorText, //
    ImmersiveStartLightTileInstallProgressFill, //
    ImmersiveStartLightTileInstallProgressTrack, //
    ImmersiveStartPrimaryText, //
    ImmersiveStartRoundButtonHover, //
    ImmersiveStartRoundButtonPressed, //
    ImmersiveStartRoundButtonRest, //
    ImmersiveStartSecondaryText, //
    ImmersiveStartSelectionBackground, //
    ImmersiveStartSelectionHoverBackground, //
    ImmersiveStartSelectionHoverPrimaryText, //
    ImmersiveStartSelectionHoverSecondaryText, //
    ImmersiveStartSelectionPrimaryText, //
    ImmersiveStartSelectionSecondaryText, //
    ImmersiveStartSystemTilesBackground, //
    ImmersiveStartThumbnailPlaceholder, //
    ImmersiveSystemAccent, //
    ImmersiveSystemAccentDark1, //
    ImmersiveSystemAccentDark2, //
    ImmersiveSystemAccentDark3, //
    ImmersiveSystemAccentLight1, //
    ImmersiveSystemAccentLight2, //
    ImmersiveSystemAccentLight3, //
    ImmersiveSystemBackground, //
    ImmersiveSystemBackgroundDarkTheme, //
    ImmersiveSystemBackgroundLightTheme, //
    ImmersiveSystemText, //
    ImmersiveSystemTextDarkTheme, //
    ImmersiveSystemTextLightTheme, //
    ImmersiveTabletModeDragJointDividerBackground, //
    ImmersiveTabletModePPIJointDividerBackground //
    );
{$ENDREGION 'ImmersiveColorType'}
  TImmersiveColors = class;

  TColorSet = class(TObject)
  private
    FId: Integer;
    FParent: TImmersiveColors;
    function AlphaToColor(AlphaColor: TAlphaColor): TColor; inline;
    function IsActive: Boolean; inline;
    function GetAlphaColor(Index: TImmersiveColorType): TAlphaColor;
    function GetColor(Index: TImmersiveColorType): TColor;
  public
    constructor Create(Parent: TImmersiveColors; Id: Integer); virtual;
    destructor Destroy; override;
    function GetAlphaColorFromColorTypeName(const ColorTypeName: String): TAlphaColor;
    function GetColorFromColorTypeName(const ColorTypeName: String): TAlphaColor;
    property Colors[Index: TImmersiveColorType]: TColor read GetColor;
    property AlphaColors[Index: TImmersiveColorType]: TAlphaColor read GetAlphaColor;
    property Active: Boolean read IsActive;
  end;

  TImmersiveColors = class(TObject)
  private
    FList: TList;
    FColorTypeNames: TStringList;
    FColorSetChangedEvent: TNotifyEvent;
    FImmersiveColorsSupported: Boolean;
    FHandle: THandle;
    function GetColorSetCount: Integer; inline;
    function GetColorSet(Index: Integer): TColorSet;
    function GetActiveColorSet: TColorSet;
    function CheckImmersiveColorsSupported: Boolean;
    procedure EnumColorTypeNames;
  protected
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Update;
    { properties }
    property ColorSetCount: Integer read GetColorSetCount;
    property ColorSets[Index: Integer]: TColorSet read GetColorSet;
    property ColorTypeNames: TStringList read FColorTypeNames;
    property ActiveColorSet: TColorSet read GetActiveColorSet;
    property IsImmersiveColorsSupported: Boolean read FImmersiveColorsSupported;
    property OnColorSetChanged: TNotifyEvent read FColorSetChangedEvent write FColorSetChangedEvent;
  end;

const
  themelib = 'uxtheme.dll';

  { ===> Undocumented UxTheme functions <=== }
{$WARNINGS Off}
function GetImmersiveColorSetCount: Integer; stdcall; external themelib index 94;
function GetImmersiveColorFromColorSetEx(dwImmersiveColorSet: UInt; dwImmersiveColorType: Integer; bIgnoreHighContrast: Bool; dwHighContrastCacheMode: UInt): UInt; stdcall; external themelib index 95;
function GetImmersiveColorTypeFromName(pName: PChar): Integer; stdcall; external themelib index 96;
function GetImmersiveUserColorSetPreference(bForceCheckRegistry: Bool; bSkipCheckOnFail: Bool): Integer; stdcall; external themelib index 98;
function GetImmersiveColorNamedTypeByIndex(dwIndex: UInt): IntPtr; stdcall; external themelib index 100;
{$WARNINGS On}

implementation

{ TImmersiveColors }

function TImmersiveColors.CheckImmersiveColorsSupported: Boolean;
var
  hModule: THandle;
begin
  hModule := GetModuleHandle(themelib);
  Result := hModule <> 0;
  if Result then
    Result := GetProcAddress(hModule, 'GetImmersiveColorFromColorSetEx') <> nil;
end;

constructor TImmersiveColors.Create;
begin
  FColorSetChangedEvent := nil;
  FColorTypeNames := nil;
  FList := nil;
  FHandle := INVALID_HANDLE_VALUE;
  FImmersiveColorsSupported := CheckImmersiveColorsSupported;
  if FImmersiveColorsSupported then
  begin
    FHandle := AllocateHWnd(WndProc);
    Update;
  end;
end;

destructor TImmersiveColors.Destroy;
var
  I: Integer;
begin
  if Assigned(FList) then
  begin
    for I := 0 to FList.Count - 1 do
      TColorSet(FList.Items[I]).Free;
    FList.Free;
  end;

  if Assigned(FColorTypeNames) then
    FColorTypeNames.Free;

  if FHandle <> INVALID_HANDLE_VALUE then
    DeallocateHWnd(FHandle);

  inherited;
end;

procedure TImmersiveColors.EnumColorTypeNames;
const
  ColorTypeNamesCount = $04C3;
var
  Id: Integer;
  I: Integer;
  P: IntPtr;
  S: String;
begin
  if Assigned(FColorTypeNames) then
  begin
    FColorTypeNames.Clear;
    for I := 0 to ColorTypeNamesCount - 1 do
    begin
      P := GetImmersiveColorNamedTypeByIndex(I);
      if (P <> 0) then
      begin
        S := String(PChar(PNativeInt(P)^)).Trim;
        if not S.IsEmpty then
        begin
          S := 'Immersive' + S;
          Id := GetImmersiveColorTypeFromName(PChar(S));
          if (Id > -1) then
            FColorTypeNames.Add(S);
        end;
      end;
    end;
  end;
end;

function TImmersiveColors.GetActiveColorSet: TColorSet;
begin
  { On my OS (Windows 10), GetImmersiveUserColorSetPreference returns 100 while
    GetImmersiveColorSetCount returns 50 !
    After some reverse engineering  I made, I found that Windows 10 stores the active
    color set at the bottom of the list !
    So in order to get the active color set, you just need to pick up the last item.
  }
  Result := nil;
  if Assigned(FList) then
    Result := FList.Items[GetColorSetCount];
end;

function TImmersiveColors.GetColorSet(Index: Integer): TColorSet;
begin
  Result := nil;
  if Assigned(FList) then
    Result := FList.Items[Index];
end;

function TImmersiveColors.GetColorSetCount: Integer;
begin
  Result := GetImmersiveColorSetCount;
end;

procedure TImmersiveColors.Update;
var
  I: Integer;
  n: Integer;
begin
  if not FImmersiveColorsSupported then
    Exit;

  if not Assigned(FList) then
    FList := TList.Create;

  for I := 0 to FList.Count - 1 do
    TColorSet(FList.Items[I]).Free;
  FList.Clear;

  n := ColorSetCount;
  for I := 0 to n do
    FList.Add(TColorSet.Create(Self, I));

  if not Assigned(FColorTypeNames) then
    FColorTypeNames := TStringList.Create;

  EnumColorTypeNames;
end;

procedure TImmersiveColors.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_SETTINGCHANGE) and Assigned(FColorSetChangedEvent) and (lstrcmpi(TWMSettingChange(Message).Section, 'ImmersiveColorSet') = 0) then
    FColorSetChangedEvent(Self);
  with Message do
    Result := DefWindowProc(FHandle, Msg, WParam, LParam);
end;

{ TColorSet }

function TColorSet.AlphaToColor(AlphaColor: TAlphaColor): TColor;
begin
  Result := TColor(AlphaColor and $00FFFFFF);
end;

constructor TColorSet.Create(Parent: TImmersiveColors; Id: Integer);
begin
  Assert(Assigned(Parent));
  Assert(Parent is TImmersiveColors);
  Assert((Id > -1) and (Id <= TImmersiveColors(Parent).ColorSetCount));
  FParent := Parent;
  FId := Id;
end;

destructor TColorSet.Destroy;
begin
  inherited;
end;

function TColorSet.GetAlphaColor(Index: TImmersiveColorType): TAlphaColor;
var
  LName: String;
begin
  LName := GetEnumName(TypeInfo(TImmersiveColorType), Ord(Index));
  Result := GetAlphaColorFromColorTypeName(LName);
end;

function TColorSet.GetColor(Index: TImmersiveColorType): TColor;
var
  AlphaColor: TAlphaColor;
begin
  AlphaColor := GetAlphaColor(Index);
  Result := AlphaToColor(AlphaColor);
end;

function TColorSet.GetColorFromColorTypeName(const ColorTypeName: String): TAlphaColor;
var
  AlphaColor: TAlphaColor;
begin
  AlphaColor := GetAlphaColorFromColorTypeName(ColorTypeName);
  Result := AlphaToColor(AlphaColor);
end;

function TColorSet.GetAlphaColorFromColorTypeName(const ColorTypeName: String): TAlphaColor;
var
  ColorType: UInt;
begin
  Result := $00;
  if not ColorTypeName.IsEmpty then
  begin
    ColorType := GetImmersiveColorTypeFromName(PChar(ColorTypeName));
    Result := GetImmersiveColorFromColorSetEx(FId, ColorType, False, 0);
  end;
end;

function TColorSet.IsActive: Boolean;
begin
  Result := FParent.ColorSetCount = FId;
end;

end.
