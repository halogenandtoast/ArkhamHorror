module Arkham.Types.Asset.Cards.ResearchLibrarian where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype ResearchLibrarian = ResearchLibrarian AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchLibrarian :: AssetId -> ResearchLibrarian
researchLibrarian uuid = ResearchLibrarian $ (baseAttrs uuid "01032")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 1
  }

instance HasModifiersFor env ResearchLibrarian where
  getModifiersFor = noModifiersFor

instance HasActions env ResearchLibrarian where
  getActions i (WhenEnterPlay target) (ResearchLibrarian x)
    | isTarget x target = pure
      [ ActivateCardAbilityAction
          i
          (mkAbility (toSource x) 1 (ReactionAbility Free))
      ]
  getActions i window (ResearchLibrarian x) = getActions i window x

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
