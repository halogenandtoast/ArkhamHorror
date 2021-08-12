module Arkham.Types.Asset.Cards.SophieInLovingMemory
  ( sophieInLovingMemory
  , SophieInLovingMemory(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype SophieInLovingMemory = SophieInLovingMemory AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieInLovingMemory :: AssetCard SophieInLovingMemory
sophieInLovingMemory = assetWith
  SophieInLovingMemory
  Cards.sophieInLovingMemory
  (canLeavePlayByNormalMeansL .~ False)

instance HasActions SophieInLovingMemory where
  getActions (SophieInLovingMemory x) =
    [ restrictedAbility x 1 (OwnsThis <> DuringSkillTest)
      $ FastAbility
      $ DirectDamageCost (toSource x) YouTarget 1
    , restrictedAbility
        x
        2
        (OwnsThis <> InvestigatorExists
          (You <> InvestigatorWithDamage (AtLeast $ Static 5))
        )
      $ ForcedAbility AnyWindow
    ]

instance HasModifiersFor env SophieInLovingMemory

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SophieInLovingMemory where
  runMessage msg a@(SophieInLovingMemory attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 2))
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (Flip (toSource attrs) (toTarget attrs))
    Flip _ target | isTarget attrs target -> do
      let
        sophieItWasAllMyFault = PlayerCard
          $ lookupPlayerCard Cards.sophieItWasAllMyFault (toCardId attrs)
        markId = fromJustNote "invalid" (assetInvestigator attrs)
      a <$ pushAll [ReplaceInvestigatorAsset markId sophieItWasAllMyFault]
    _ -> SophieInLovingMemory <$> runMessage msg attrs
