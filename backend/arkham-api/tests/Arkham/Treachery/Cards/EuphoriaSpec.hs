module Arkham.Treachery.Cards.EuphoriaSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Placement (Placement (InPlayArea, InThreatArea))
import Arkham.SkillTest.Type (SkillTestType (..))
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Euphoria" $ do
  it "does not allow permanent assets to be placed on The Abyss" . gameTest $ \self -> do
    charisma <- genCard Assets.charisma3
    loneWolf <- genCard Assets.loneWolf
    euphoria <- genCard Treacheries.euphoria
    (charismaId, createCharisma) <- Helpers.createAssetAt charisma (InPlayArea self.id)
    (loneWolfId, createLoneWolf) <- Helpers.createAssetAt loneWolf (InPlayArea self.id)
    (euphoriaId, createEuphoria) <- Helpers.createTreacheryAt euphoria (InThreatArea self.id)

    runAll [createCharisma, createLoneWolf, createEuphoria]
    run
      $ FailedSkillTest
        self.id
        Nothing
        (TreacherySource euphoriaId)
        (SkillTestInitiatorTarget $ TreacheryTarget euphoriaId)
        (SkillSkillTest #intellect)
        1
    applyAllDamage

    assertNotTarget charismaId
    assertTarget loneWolfId
