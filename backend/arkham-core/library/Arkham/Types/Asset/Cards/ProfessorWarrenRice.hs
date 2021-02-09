module Arkham.Types.Asset.Cards.ProfessorWarrenRice where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetId -> ProfessorWarrenRice
professorWarrenRice uuid = ProfessorWarrenRice $ (baseAttrs uuid "02061")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 3
  }

instance HasModifiersFor env ProfessorWarrenRice where
  getModifiersFor _ (InvestigatorTarget iid) (ProfessorWarrenRice a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ProfessorWarrenRice where
  getActions iid (AfterDiscoveringClues You YourLocation) (ProfessorWarrenRice a@AssetAttrs {..})
    = do
      lid <- getId @LocationId iid
      lastClue <- (== 0) . unClueCount <$> getCount lid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource a)
              1
              (ReactionAbility $ ExhaustCost (toTarget a))
            )
        | lastClue && ownedBy a iid
        ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (DrawCards iid 1 False)
    _ -> ProfessorWarrenRice <$> runMessage msg attrs
