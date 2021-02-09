module Arkham.Types.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
  ) where


import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.Event.Attrs
import Arkham.Types.Trait

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astoundingRevelation :: InvestigatorId -> EventId -> AstoundingRevelation
astoundingRevelation iid uuid =
  AstoundingRevelation $ baseAttrs iid uuid "06023"

ability :: InvestigatorId -> EventAttrs -> Ability
ability iid a = base
  { abilityLimit = PlayerLimit (PerSearch $ Just Research) 1
  }
 where
  base = mkAbility
    (toSource a)
    1
    (ReactionAbility (DiscardCost (SearchedCardTarget iid $ getCardId a)))

instance HasActions env AstoundingRevelation where
  getActions iid (WhenAmongSearchedCards You) (AstoundingRevelation attrs) =
    pure [ActivateCardAbilityAction iid (ability iid attrs)]
  getActions iid window (AstoundingRevelation attrs) =
    getActions iid window attrs

instance HasModifiersFor env AstoundingRevelation where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasSet AssetId env (InvestigatorId, UseType)) => RunMessage env AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      secretAssetIds <- getSetList (iid, Secret)
      e <$ unshiftMessage
        (chooseOne
          iid
          (TakeResources iid 2 False
          : [ AddUses (AssetTarget aid) Secret 1 | aid <- secretAssetIds ]
          )
        )
    _ -> AstoundingRevelation <$> runMessage msg attrs
