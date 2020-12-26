{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Blackjack
  ( blackjack
  , Blackjack(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype Blackjack = Blackjack Attrs
  deriving newtype (Show, ToJSON, FromJSON)

blackjack :: AssetId -> Blackjack
blackjack uuid =
  Blackjack $ (baseAttrs uuid "02016") { assetSlots = [HandSlot] }

instance ActionRunner env => HasActions env Blackjack where
  getActions iid window (Blackjack a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env Blackjack where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Blackjack where
  runMessage msg a@(Blackjack attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateSkillTestEffect
          (EffectModifiers $ toModifiers
            attrs
            [SkillModifier SkillCombat 1, DoesNotDamageOtherInvestigator]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
    _ -> Blackjack <$> runMessage msg attrs
