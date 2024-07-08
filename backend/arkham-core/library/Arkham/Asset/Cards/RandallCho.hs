module Arkham.Asset.Cards.RandallCho (randallCho, RandallCho (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (InvestigatorDamage)
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait
import Arkham.Zone qualified as Zone

newtype RandallCho = RandallCho AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallCho :: AssetCard RandallCho
randallCho = ally RandallCho Cards.randallCho (1, 3)

instance HasAbilities RandallCho where
  getAbilities (RandallCho x) =
    [restrictedAbility x 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be x)]

instance RunMessage RandallCho where
  runMessage msg a@(RandallCho attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      damage <- canHaveDamageHealed attrs iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ catMaybes
          [ Label
              "Heal 3 damage"
              [HealDamage (InvestigatorTarget iid) (toSource attrs) 3]
              <$ guard damage
          , Just
              $ Label
                "Search your deck and discard pile for a Weapon asset, play it (paying its cost), and shuffle your deck"
                [ search
                    iid
                    (attrs.ability 1)
                    iid
                    [(Zone.FromDeck, ShuffleBackIn), (Zone.FromDiscard, PutBack)]
                    (basic $ #asset <> withTrait Weapon)
                    (PlayFound iid 1)
                ]
          ]

      pure a
    _ -> RandallCho <$> runMessage msg attrs
