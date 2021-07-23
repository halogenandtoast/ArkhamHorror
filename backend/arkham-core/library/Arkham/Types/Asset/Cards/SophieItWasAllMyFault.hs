module Arkham.Types.Asset.Cards.SophieItWasAllMyFault
  ( sophieItWasAllMyFault
  , SophieItWasAllMyFault(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target

newtype SophieItWasAllMyFault = SophieItWasAllMyFault AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieItWasAllMyFault :: AssetCard SophieItWasAllMyFault
sophieItWasAllMyFault = assetWith
  SophieItWasAllMyFault
  Cards.sophieItWasAllMyFault
  (canLeavePlayByNormalMeansL .~ False)

ability :: AssetAttrs -> Ability
ability attrs = mkAbility attrs 1 ForcedAbility & abilityLimitL .~ NoLimit

instance HasCount DamageCount env InvestigatorId => HasActions env SophieItWasAllMyFault where
  getActions iid _ (SophieItWasAllMyFault attrs) = whenOwnedBy attrs iid $ do
    damageCount <- unDamageCount <$> getCount iid
    pure [ UseAbility iid (ability attrs) | damageCount <= 4 ]

instance HasModifiersFor env SophieItWasAllMyFault where
  getModifiersFor _ (InvestigatorTarget iid) (SophieItWasAllMyFault attrs)
    | ownedBy attrs iid = pure $ toModifiers attrs [AnySkillValue (-1)]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SophieItWasAllMyFault where
  runMessage msg a@(SophieItWasAllMyFault attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (Flip (toSource attrs) (toTarget attrs))
    Flip _ target | isTarget attrs target -> do
      let
        sophieInLovingMemory = PlayerCard
          $ lookupPlayerCard Cards.sophieInLovingMemory (toCardId attrs)
        markId = fromJustNote "invalid" (assetInvestigator attrs)
      a <$ pushAll [ReplaceInvestigatorAsset markId sophieInLovingMemory]
    _ -> SophieItWasAllMyFault <$> runMessage msg attrs
