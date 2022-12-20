module Arkham.Asset.Cards.RandallCho
  ( randallCho
  , RandallCho(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( InvestigatorDamage )
import Arkham.Card.CardType
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Zone qualified as Zone

newtype RandallCho = RandallCho AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallCho :: AssetCard RandallCho
randallCho = ally RandallCho Cards.randallCho (1, 3)

instance HasAbilities RandallCho where
  getAbilities (RandallCho x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
            (AssetEntersPlay Timing.After $ AssetWithId $ toId x)
            Free
    ]

instance RunMessage RandallCho where
  runMessage msg a@(RandallCho attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      damage <- field InvestigatorDamage iid
      push $ chooseOrRunOne iid $ catMaybes
        [ Label
            "Heal 3 damage"
            [HealDamage (InvestigatorTarget iid) (toSource attrs) 3]
          <$ guard (damage > 0)
        , Just $ Label
          "Search your deck and discard pile for a Weapon asset, play it (paying its cost), and shuffle your deck"
          [ Search
              iid
              (toSource attrs)
              (InvestigatorTarget iid)
              [(Zone.FromDeck, ShuffleBackIn), (Zone.FromDiscard, PutBack)]
              (CardWithType AssetType <> CardWithTrait Weapon)
              (PlayFound iid 1)
          ]
        ]

      pure a
    _ -> RandallCho <$> runMessage msg attrs
