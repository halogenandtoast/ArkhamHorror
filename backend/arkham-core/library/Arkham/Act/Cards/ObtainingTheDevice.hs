module Arkham.Act.Cards.ObtainingTheDevice (
  ObtainingTheDevice (..),
  obtainingTheDevice,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ObtainingTheDevice = ObtainingTheDevice ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

obtainingTheDevice :: ActCard ObtainingTheDevice
obtainingTheDevice = act (2, A) ObtainingTheDevice Cards.obtainingTheDevice Nothing

instance HasAbilities ObtainingTheDevice where
  getAbilities (ObtainingTheDevice attrs) =
    [ mkAbility attrs 1
        $ Objective
        $ ForcedAbility
        $ AddedToVictory Timing.AtIf
        $ cardIs Enemies.nathanWickMasterOfInitiation
    ]

instance RunMessage ObtainingTheDevice where
  runMessage msg a@(ObtainingTheDevice attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      puzzleBox <- selectJust $ assetIs Assets.puzzleBox
      investigators <- getInvestigators
      lead <- getLeadPlayer
      pushAll
        [ chooseOrRunOne
            lead
            [ targetLabel investigator [TakeControlOfAsset investigator puzzleBox] | investigator <- investigators
            ]
        , AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
        ]

      pure a
    AdvanceAct actId _ _ | toId attrs == actId && onSide B attrs -> do
      push $ advanceActDeck attrs
      pure a
    _ -> ObtainingTheDevice <$> runMessage msg attrs
