module Arkham.Types.Asset.Cards.RandallCho
  ( randallCho
  , RandallCho(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Zone qualified as Zone

newtype RandallCho = RandallCho AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randallCho :: AssetCard RandallCho
randallCho = ally RandallCho Cards.randallCho (1, 3)

instance HasAbilities RandallCho where
  getAbilities (RandallCho x) =
    [ restrictedAbility x 1 OwnsThis
        $ ReactionAbility
            (AssetEntersPlay Timing.After $ AssetWithId $ toId x)
            Free
    ]

instance AssetRunner env => RunMessage env RandallCho where
  runMessage msg a@(RandallCho attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      damage <- unDamageCount <$> getCount iid
      push $ chooseOrRunOne
        iid
        (catMaybes
          [ Label "Heal 3 damage" [HealDamage (InvestigatorTarget iid) 3]
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
        )
      pure a
    _ -> RandallCho <$> runMessage msg attrs
