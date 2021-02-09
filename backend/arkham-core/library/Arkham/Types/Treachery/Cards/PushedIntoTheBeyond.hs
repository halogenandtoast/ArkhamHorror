module Arkham.Types.Treachery.Cards.PushedIntoTheBeyond
  ( PushedIntoTheBeyond(..)
  , pushedIntoTheBeyond
  )
where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryId -> a -> PushedIntoTheBeyond
pushedIntoTheBeyond uuid _ = PushedIntoTheBeyond $ baseAttrs uuid "02100"

instance HasModifiersFor env PushedIntoTheBeyond where
  getModifiersFor = noModifiersFor

instance HasActions env PushedIntoTheBeyond where
  getActions i window (PushedIntoTheBeyond attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      storyAssets <- map unStoryAssetId <$> getSetList iid
      validAssets <- filter (`notElem` storyAssets) <$> getSetList iid
      targets <- traverse (traverseToSnd getId) validAssets
      t <$ unshiftMessage
        (chooseOne
          iid
          [ TargetLabel
              (AssetTarget aid)
              [ ShuffleIntoDeck iid (AssetTarget aid)
              , CreateEffect
                (CardCode "02100")
                (Just (EffectCardCode cardCode))
                (toSource attrs)
                (InvestigatorTarget iid)
              , Discard (toTarget attrs)
              ]
          | (aid, cardCode) <- targets
          ]
        )
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
