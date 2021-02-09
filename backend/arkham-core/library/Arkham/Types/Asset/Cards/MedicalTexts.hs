module Arkham.Types.Asset.Cards.MedicalTexts
  ( MedicalTexts(..)
  , medicalTexts
  )
where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype MedicalTexts = MedicalTexts AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medicalTexts :: AssetId -> MedicalTexts
medicalTexts uuid =
  MedicalTexts $ (baseAttrs uuid "01035") { assetSlots = [HandSlot] }

instance HasModifiersFor env MedicalTexts where
  getModifiersFor = noModifiersFor

instance HasActions env MedicalTexts where
  getActions iid NonFast (MedicalTexts a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env MedicalTexts where
  runMessage msg a@(MedicalTexts attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationInvestigatorIds <- getSetList locationId
      unshiftMessage
        (chooseOne
          iid
          [ BeginSkillTest
              iid
              source
              (InvestigatorTarget iid')
              Nothing
              SkillIntellect
              2
          | iid' <- locationInvestigatorIds
          ]
        )
      MedicalTexts <$> runMessage msg attrs
    PassedSkillTest _ _ source target _ _ | isSource attrs source ->
      a <$ unshiftMessage (HealDamage target 1)
    FailedSkillTest _ _ source (InvestigatorTarget iid) _ _
      | isSource attrs source -> a
      <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny 1 0)
    _ -> MedicalTexts <$> runMessage msg attrs
