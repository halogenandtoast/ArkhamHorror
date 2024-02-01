module Arkham.Asset.Cards.SophieInLovingMemory (
  sophieInLovingMemory,
  SophieInLovingMemory (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Matcher

newtype SophieInLovingMemory = SophieInLovingMemory AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

sophieInLovingMemory :: AssetCard SophieInLovingMemory
sophieInLovingMemory =
  assetWith
    SophieInLovingMemory
    Cards.sophieInLovingMemory
    (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities SophieInLovingMemory where
  getAbilities (SophieInLovingMemory x) =
    [ restrictedAbility
        x
        1
        (ControlsThis <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ FastAbility
        $ DirectDamageCost (toSource x) You 1
    , restrictedAbility
        x
        2
        ( ControlsThis
            <> InvestigatorExists
              (You <> InvestigatorWithDamage (AtLeast $ Static 5))
        )
        $ ForcedAbility AnyWindow
    ]

instance RunMessage SophieInLovingMemory where
  runMessage msg a@(SophieInLovingMemory attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              (skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 2))
    UseCardAbility iid source 2 _ _
      | isSource attrs source ->
          a <$ push (Flip iid (toSource attrs) (toTarget attrs))
    Flip _ _ target | isTarget attrs target -> do
      let
        sophieItWasAllMyFault =
          PlayerCard
            $ lookupPlayerCard Cards.sophieItWasAllMyFault (toCardId attrs)
        markId = getController attrs
      a <$ pushAll [ReplaceInvestigatorAsset markId sophieItWasAllMyFault]
    _ -> SophieInLovingMemory <$> runMessage msg attrs
