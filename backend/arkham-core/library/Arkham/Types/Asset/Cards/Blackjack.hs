module Arkham.Types.Asset.Cards.Blackjack
  ( blackjack
  , Blackjack(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Blackjack = Blackjack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackjack :: AssetCard Blackjack
blackjack = hand Blackjack Cards.blackjack

instance HasActions Blackjack where
  getActions (Blackjack a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance HasModifiersFor env Blackjack

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Blackjack where
  runMessage msg a@(Blackjack attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [SkillModifier SkillCombat 1, DoesNotDamageOtherInvestigator]
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> Blackjack <$> runMessage msg attrs
