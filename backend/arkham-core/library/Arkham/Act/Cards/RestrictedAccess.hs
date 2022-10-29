module Arkham.Act.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.ScenarioLogKey

newtype RestrictedAccess = RestrictedAccess ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

restrictedAccess :: ActCard RestrictedAccess
restrictedAccess = act (2, A) RestrictedAccess Cards.restrictedAccess Nothing

instance HasAbilities RestrictedAccess where
  getAbilities (RestrictedAccess a) =
    [ restrictedAbility
          a
          1
          (AtLeastNCriteriaMet
            3
            [ AssetExists (assetIs Assets.theCustodian <> ControlledAsset)
            , Remembered FoundTheProcess
            , Remembered DissectedAnOrgan
            , Remembered InterviewedASubject
            , Remembered RealizedWhatYearItIs
            , Remembered ActivatedTheDevice
            ]
          )
        $ Objective
        $ ForcedAbility AnyWindow
    | onSide A a
    ]

instance RunMessage RestrictedAccess where
  runMessage msg a@(RestrictedAccess attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      pushAll
        [ AddToVictory (toTarget attrs)
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> RestrictedAccess <$> runMessage msg attrs
