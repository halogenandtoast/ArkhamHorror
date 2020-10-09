{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Duke
  ( Duke(..)
  , duke
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Duke = Duke Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

duke :: AssetId -> Duke
duke uuid =
  Duke $ (baseAttrs uuid "02014") { assetHealth = Just 2, assetSanity = Just 3 }

instance IsInvestigator investigator => HasModifiersFor env investigator Duke where
  getModifiersFor (SkillTestSource source (Just Action.Fight)) i (Duke a)
    | ownedBy a i && isSource a source = pure
      [BaseSkillOf SkillCombat 4, DamageDealt 1]
  getModifiersFor (SkillTestSource source (Just Action.Investigate)) i (Duke a)
    | ownedBy a i && isSource a source = pure [BaseSkillOf SkillIntellect 4]
  getModifiersFor _ _ _ = pure []

fightAbility :: Attrs -> Ability
fightAbility attrs =
  mkAbility (toSource attrs) 1 (ActionAbility 1 (Just Action.Fight))

investigateAbility :: Attrs -> Ability
investigateAbility attrs =
  mkAbility (toSource attrs) 2 (ActionAbility 1 (Just Action.Investigate))

instance (ActionRunner env investigator) => HasActions env investigator Duke where
  getActions i NonFast (Duke a) | ownedBy a i = do
    fightAvailable <- hasFightActions i NonFast
    investigateAvailable <- hasInvestigateActions i NonFast
    pure
      $ [ ActivateCardAbilityAction (getId () i) (fightAbility a)
        | fightAvailable && canDo Action.Fight i && not (assetExhausted a)
        ]
      <> [ ActivateCardAbilityAction (getId () i) (investigateAbility a)
         | investigateAvailable && canDo Action.Investigate i && not
           (assetExhausted a)
         ]
  getActions i window (Duke x) = getActions i window x

dukeInvestigate :: Attrs -> InvestigatorId -> LocationId -> Message
dukeInvestigate attrs iid lid =
  Investigate iid lid (toSource attrs) SkillIntellect mempty mempty mempty False

instance (AssetRunner env) => RunMessage env Duke where
  runMessage msg (Duke attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage
        $ ChooseFightEnemy iid source SkillCombat mempty mempty False
      pure . Duke $ attrs & exhausted .~ True
    UseCardAbility iid source _ 2 | isSource attrs source -> do
      lid <- asks $ getId iid
      accessibleLocationIds <-
        asks $ map unAccessibleLocationId . setToList . getSet lid
      if null accessibleLocationIds
        then unshiftMessage $ dukeInvestigate attrs iid lid
        else unshiftMessage
          (chooseOne iid
          $ dukeInvestigate attrs iid lid
          : [ Run [MoveAction iid lid' False, dukeInvestigate attrs iid lid']
            | lid' <- accessibleLocationIds
            ]
          )
      pure . Duke $ attrs & exhausted .~ True
    _ -> Duke <$> runMessage msg attrs
