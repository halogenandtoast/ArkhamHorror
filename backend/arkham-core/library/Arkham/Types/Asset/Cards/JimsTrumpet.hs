{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.JimsTrumpet
  ( JimsTrumpet(..)
  , jimsTrumpet
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype JimsTrumpet = JimsTrumpet Attrs
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

jimsTrumpet :: AssetId -> JimsTrumpet
jimsTrumpet uuid = JimsTrumpet $ baseAttrs uuid "02012" $ slots .= [HandSlot]

instance HasModifiersFor env JimsTrumpet where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Who -> Ability
ability attrs who =
  mkAbility (toSource attrs) 1 (ReactionAbility (WhenDrawToken who Skull))

instance ActionRunner env => HasActions env JimsTrumpet where
  getActions iid (WhenRevealToken who Skull) (JimsTrumpet a) | ownedBy a iid =
    do
      let ability' = (iid, ability a who)
      locationId <- asks $ getId @LocationId iid
      connectedLocationIds <-
        asks $ map unConnectedLocationId . setToList . getSet locationId
      investigatorIds <- for
        (locationId : connectedLocationIds)
        (asks . (setToList .) . getSet @InvestigatorId)
      horrorCounts <- for
        (concat investigatorIds)
        (asks . (unHorrorCount .) . getCount)
      unused <- asks $ notElem ability' . map unUsedAbility . getList ()
      pure
        [ uncurry ActivateCardAbilityAction ability'
        | unused && any (> 0) horrorCounts
        ]
  getActions i window (JimsTrumpet x) = getActions i window x

instance (AssetRunner env) => RunMessage env JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@Attrs {..}) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source -> do
      let ownerId = fromJustNote "must be owned" assetInvestigator
      locationId <- asks $ getId ownerId
      connectedLocationIds <-
        asks $ map unConnectedLocationId . setToList . getSet locationId
      investigatorIds <- concat <$> for
        (locationId : connectedLocationIds)
        (asks . (setToList .) . getSet)
      pairings <- for investigatorIds
        $ \targetId -> asks $ (targetId, ) . unHorrorCount . getCount targetId
      let choices = map fst $ filter ((> 0) . snd) pairings
      a <$ unshiftMessage
        (chooseOne
          ownerId
          [ HealHorror (InvestigatorTarget iid) 1 | iid <- choices ]
        )
    _ -> JimsTrumpet <$> runMessage msg attrs
