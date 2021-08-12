module Arkham.Types.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astoundingRevelation :: EventCard AstoundingRevelation
astoundingRevelation = event AstoundingRevelation Cards.astoundingRevelation

instance HasActions AstoundingRevelation where
  getActions (AstoundingRevelation x) =
    [ restrictedAbility
          x
          1
          OwnsThis
          (ReactionAbility
            (AmongSearchedCards Timing.When You (CardWithId $ toCardId x))
            (DiscardCost (SearchedCardTarget $ toCardId x))
          )
        & abilityLimitL
        .~ PlayerLimit (PerSearch $ Just Research) 1
    ]

instance HasModifiersFor env AstoundingRevelation

instance (Query AssetMatcher env, HasQueue env) => RunMessage env AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      secretAssetIds <- selectList (AssetOwnedBy You <> AssetWithUseType Secret)
      e <$ push
        (chooseOne
          iid
          (TakeResources iid 2 False
          : [ AddUses (AssetTarget aid) Secret 1 | aid <- secretAssetIds ]
          )
        )
    _ -> AstoundingRevelation <$> runMessage msg attrs
