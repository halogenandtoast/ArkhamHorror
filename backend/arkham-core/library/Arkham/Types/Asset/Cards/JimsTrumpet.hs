module Arkham.Types.Asset.Cards.JimsTrumpet
  ( JimsTrumpet(..)
  , jimsTrumpet
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype JimsTrumpet = JimsTrumpet AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

jimsTrumpet :: AssetId -> JimsTrumpet
jimsTrumpet uuid =
  JimsTrumpet $ (baseAttrs uuid "02012") { assetSlots = [HandSlot] }

instance HasModifiersFor env JimsTrumpet where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility $ ExhaustCost (toTarget attrs))

instance ActionRunner env => HasActions env JimsTrumpet where
  getActions iid (WhenRevealToken _ Skull) (JimsTrumpet a) | ownedBy a iid = do
    locationId <- getId @LocationId iid
    connectedLocationIds <- map unConnectedLocationId <$> getSetList locationId
    investigatorIds <- for
      (locationId : connectedLocationIds)
      (getSetList @InvestigatorId)
    horrorCounts <- for
      (concat investigatorIds)
      ((unHorrorCount <$>) . getCount)
    pure [ ActivateCardAbilityAction iid (ability a) | any (> 0) horrorCounts ]
  getActions i window (JimsTrumpet x) = getActions i window x

instance AssetRunner env => RunMessage env JimsTrumpet where
  runMessage msg a@(JimsTrumpet attrs@AssetAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      let ownerId = fromJustNote "must be owned" assetInvestigator
      locationId <- getId ownerId
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList locationId
      investigatorIds <-
        concat <$> for (locationId : connectedLocationIds) getSetList
      pairings <- for investigatorIds
        $ \targetId -> (targetId, ) . unHorrorCount <$> getCount targetId
      let choices = map fst $ filter ((> 0) . snd) pairings
      a <$ unshiftMessage
        (chooseOne
          ownerId
          [ HealHorror (InvestigatorTarget iid) 1 | iid <- choices ]
        )
    _ -> JimsTrumpet <$> runMessage msg attrs
