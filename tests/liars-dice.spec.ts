import { test, expect } from '@playwright/test';

const BASE_URL = 'https://anuar-burkitbayev.github.io/liars-dice'; // Adjust port as needed

// Tests for AI Mode
test.describe('Liar\'s Dice AI Mode', () => {
  
  test('has correct title', async ({ page }) => {
    await page.goto(BASE_URL);
    await expect(page).toHaveTitle(/Liar's Dice/);
  });

  test('displays AI mode button on title screen', async ({ page }) => {
    await page.goto(BASE_URL);
    
    // Wait for the title screen
    await page.waitForSelector('.title-screen');
    
    // Check for AI mode button
    const aiButton = page.locator('button').filter({ hasText: /AI/i });
    await expect(aiButton.first()).toBeVisible();
  });

  test('can start AI game mode', async ({ page }) => {
    await page.goto(BASE_URL);
    
    // Wait for title screen
    await page.waitForSelector('.title-screen');
    
    // Click AI mode button
    const aiButton = page.locator('button').filter({ hasText: /AI/i }).first();
    await aiButton.click();
    
    // Verify game container is visible
    await expect(page.locator('.game-container')).toBeVisible({ timeout: 3000 });
  });

  test('displays dice when AI game is active', async ({ page }) => {
    await page.goto(BASE_URL);
    
    // Start AI game
    await page.waitForSelector('.title-screen');
    const aiButton = page.locator('button').filter({ hasText: /AI/i }).first();
    await aiButton.click();
    await page.waitForTimeout(1000);
    
    // Check for dice elements
    const diceElements = page.locator('.die');
    await expect(diceElements.first()).toBeVisible();
    
    // Verify dice faces are rendered
    const diceText = await diceElements.first().textContent();
    expect(diceText).toMatch(/[⚀⚁⚂⚃⚄⚅?]/);
  });

  test('opponent dice are hidden during play', async ({ page }) => {
    await page.goto(BASE_URL);
    
    // Start AI game
    await page.waitForSelector('.title-screen');
    const aiButton = page.locator('button').filter({ hasText: /AI/i }).first();
    await aiButton.click();
    await page.waitForTimeout(1000);
    
    // Check for hidden dice
    const hiddenDice = page.locator('.die.hidden');
    if (await hiddenDice.count() > 0) {
      await expect(hiddenDice.first()).toBeVisible();
      const text = await hiddenDice.first().textContent();
      expect(text).toBe('?');
    }
  });
});
